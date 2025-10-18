/**
 * C Recipe - Switchback HTTP Server
 * A simple HTTP server demonstrating Switchback integration with pure C
 * Demonstrates OPTIMISTIC UPDATES with a todo list API
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <errno.h>
#include <signal.h>
#include <time.h>

#define PORT 8000
#define BUFFER_SIZE 8192
#define MAX_HEADERS 50

// Global server state
static volatile int server_running = 1;
static int server_fd_global = -1;

// Todo structure
typedef struct Todo {
    int id;
    char text[256];
    int completed;
    int likes;
    struct Todo *next;
} Todo;

// Global todos state (thread-safe with mutex)
static Todo *todos_head = NULL;
static int next_todo_id = 1;
static int total_likes = 0;
static pthread_mutex_t todos_mutex = PTHREAD_MUTEX_INITIALIZER;

// HTTP request structure
typedef struct {
    char method[16];
    char uri[256];
    int is_switchback;
    int content_length;
    char *body;
} HttpRequest;

// Utility: Send HTTP response
void send_response(int client_fd, const char *status, const char *content_type, const char *body) {
    char header[512];
    int body_len = body ? strlen(body) : 0;

    snprintf(header, sizeof(header),
        "HTTP/1.1 %s\r\n"
        "Content-Type: %s\r\n"
        "Content-Length: %d\r\n"
        "\r\n",
        status, content_type, body_len);

    write(client_fd, header, strlen(header));
    if (body) {
        write(client_fd, body, body_len);
    }
}

// Read entire file into memory
char *read_file(const char *path, size_t *size) {
    FILE *f = fopen(path, "rb");
    if (!f) return NULL;

    fseek(f, 0, SEEK_END);
    *size = ftell(f);
    fseek(f, 0, SEEK_SET);

    char *content = malloc(*size + 1);
    if (!content) {
        fclose(f);
        return NULL;
    }

    fread(content, 1, *size, f);
    content[*size] = '\0';
    fclose(f);

    return content;
}

// Serve static files
void serve_static(int client_fd, const char *path) {
    // Skip leading slash
    if (path[0] == '/') path++;

    size_t size;
    char *content = read_file(path, &size);

    if (!content) {
        send_response(client_fd, "404 Not Found", "text/plain", "404 Not Found");
        return;
    }

    // Determine content type
    const char *content_type = "application/octet-stream";
    if (strstr(path, ".js")) content_type = "application/javascript";
    else if (strstr(path, ".css")) content_type = "text/css";
    else if (strstr(path, ".html")) content_type = "text/html";

    send_response(client_fd, "200 OK", content_type, content);
    free(content);
}

// Escape JSON string
char *json_escape(const char *str) {
    size_t len = strlen(str);
    char *escaped = malloc(len * 2 + 1);
    size_t j = 0;

    for (size_t i = 0; i < len; i++) {
        switch (str[i]) {
            case '"': escaped[j++] = '\\'; escaped[j++] = '"'; break;
            case '\\': escaped[j++] = '\\'; escaped[j++] = '\\'; break;
            case '\n': escaped[j++] = '\\'; escaped[j++] = 'n'; break;
            case '\r': escaped[j++] = '\\'; escaped[j++] = 'r'; break;
            case '\t': escaped[j++] = '\\'; escaped[j++] = 't'; break;
            default: escaped[j++] = str[i];
        }
    }
    escaped[j] = '\0';

    return escaped;
}

// Count todos (must hold lock)
int count_todos() {
    int count = 0;
    Todo *current = todos_head;
    while (current) {
        count++;
        current = current->next;
    }
    return count;
}

// Build JSON array of todos (must hold lock)
void build_todos_json(char *buffer, size_t buffer_size) {
    buffer[0] = '\0';
    strcat(buffer, "[");

    Todo *current = todos_head;
    int first = 1;
    while (current) {
        if (!first) strcat(buffer, ",");
        first = 0;

        char *escaped_text = json_escape(current->text);
        char todo_json[512];
        snprintf(todo_json, sizeof(todo_json),
            "{\"id\":%d,\"text\":\"%s\",\"completed\":%s,\"likes\":%d}",
            current->id, escaped_text,
            current->completed ? "true" : "false",
            current->likes);
        strcat(buffer, todo_json);
        free(escaped_text);

        current = current->next;
    }

    strcat(buffer, "]");
}

// Find todo by ID (must hold lock)
Todo *find_todo(int id) {
    Todo *current = todos_head;
    while (current) {
        if (current->id == id) return current;
        current = current->next;
    }
    return NULL;
}

// Parse JSON body to extract field
char *extract_json_field(const char *json, const char *field) {
    char search[128];
    snprintf(search, sizeof(search), "\"%s\":", field);

    const char *start = strstr(json, search);
    if (!start) return NULL;

    start += strlen(search);
    while (*start == ' ' || *start == '\t') start++;

    if (*start == '"') {
        // String value
        start++;
        const char *end = strchr(start, '"');
        if (!end) return NULL;

        size_t len = end - start;
        char *value = malloc(len + 1);
        memcpy(value, start, len);
        value[len] = '\0';
        return value;
    } else if (*start == 't' || *start == 'f') {
        // Boolean value
        if (strncmp(start, "true", 4) == 0) {
            return strdup("true");
        } else if (strncmp(start, "false", 5) == 0) {
            return strdup("false");
        }
    }

    return NULL;
}

// Handle API routes
char *handle_api_route(HttpRequest *req) {
    char *response = malloc(4096);

    // POST /api/todos - Add new todo
    if (strcmp(req->uri, "/api/todos") == 0 && strcmp(req->method, "POST") == 0) {
        char *text = extract_json_field(req->body, "text");
        if (!text || strlen(text) == 0) {
            snprintf(response, 4096, "{\"error\":\"Text is required\"}");
            free(text);
            return response;
        }

        pthread_mutex_lock(&todos_mutex);

        // Create new todo
        Todo *new_todo = malloc(sizeof(Todo));
        new_todo->id = next_todo_id++;
        strncpy(new_todo->text, text, sizeof(new_todo->text) - 1);
        new_todo->text[sizeof(new_todo->text) - 1] = '\0';
        new_todo->completed = 0;
        new_todo->likes = 0;
        new_todo->next = todos_head;
        todos_head = new_todo;

        // Simulate delay for demo
        usleep(300000); // 300ms delay

        char *escaped_text = json_escape(new_todo->text);
        snprintf(response, 4096,
            "{\"todo\":{\"id\":%d,\"text\":\"%s\",\"completed\":false,\"likes\":0}}",
            new_todo->id, escaped_text);
        free(escaped_text);

        pthread_mutex_unlock(&todos_mutex);
        free(text);

        printf("✓ Added todo: %s\n", new_todo->text);
        return response;
    }

    // POST /api/todos/:id/like - Toggle like
    if (strncmp(req->uri, "/api/todos/", 11) == 0 && strstr(req->uri, "/like") && strcmp(req->method, "POST") == 0) {
        int todo_id = atoi(req->uri + 11);
        char *liked_str = extract_json_field(req->body, "liked");
        int liked = (liked_str && strcmp(liked_str, "true") == 0);
        free(liked_str);

        pthread_mutex_lock(&todos_mutex);

        Todo *todo = find_todo(todo_id);
        if (!todo) {
            pthread_mutex_unlock(&todos_mutex);
            snprintf(response, 4096, "{\"error\":\"Todo not found\"}");
            return response;
        }

        // Simulate delay for demo
        usleep(200000); // 200ms delay

        if (liked) {
            todo->likes++;
            total_likes++;
        } else {
            todo->likes--;
            total_likes--;
        }

        snprintf(response, 4096, "{\"likes\":%d}", todo->likes);

        pthread_mutex_unlock(&todos_mutex);

        printf("👍 Todo %d: %d likes\n", todo_id, todo->likes);
        return response;
    }

    // DELETE /api/todos/:id - Delete todo
    if (strncmp(req->uri, "/api/todos/", 11) == 0 && strcmp(req->method, "DELETE") == 0) {
        int todo_id = atoi(req->uri + 11);

        pthread_mutex_lock(&todos_mutex);

        Todo *prev = NULL;
        Todo *current = todos_head;
        while (current) {
            if (current->id == todo_id) {
                if (prev) {
                    prev->next = current->next;
                } else {
                    todos_head = current->next;
                }

                total_likes -= current->likes;
                printf("🗑️  Deleted todo: %s\n", current->text);
                free(current);

                pthread_mutex_unlock(&todos_mutex);
                snprintf(response, 4096, "{\"success\":true}");
                return response;
            }
            prev = current;
            current = current->next;
        }

        pthread_mutex_unlock(&todos_mutex);
        snprintf(response, 4096, "{\"error\":\"Todo not found\"}");
        return response;
    }

    snprintf(response, 4096, "{\"error\":\"Not found\"}");
    return response;
}

// Handle page routes
char *handle_page_route(HttpRequest *req) {
    char *json = malloc(8192);

    if (strcmp(req->uri, "/") == 0) {
        pthread_mutex_lock(&todos_mutex);
        int todo_count = count_todos();
        int likes = total_likes;
        pthread_mutex_unlock(&todos_mutex);

        snprintf(json, 8192,
            "{\"component\":\"Home\",\"props\":{\"stats\":{\"todos\":%d,\"totalLikes\":%d,\"framework\":\"C99\"}},\"url\":\"/\"}",
            todo_count, likes);
    } else if (strcmp(req->uri, "/todos") == 0) {
        pthread_mutex_lock(&todos_mutex);

        char todos_json[4096];
        build_todos_json(todos_json, sizeof(todos_json));

        pthread_mutex_unlock(&todos_mutex);

        snprintf(json, 8192,
            "{\"component\":\"Todos\",\"props\":{\"todos\":%s},\"url\":\"/todos\"}",
            todos_json);
    } else if (strcmp(req->uri, "/about") == 0) {
        snprintf(json, 8192,
            "{\"component\":\"About\",\"props\":{\"version\":\"1.0.0\",\"backend\":\"C99\",\"features\":[\"Optimistic updates\",\"Multi-threaded HTTP server\",\"In-memory todo storage\",\"Manual memory management\",\"POSIX sockets\"]},\"url\":\"/about\"}");
    } else {
        snprintf(json, 8192,
            "{\"component\":\"Error\",\"props\":{\"message\":\"Page not found\"},\"url\":\"%s\"}",
            req->uri);
    }

    return json;
}

// Respond with JSON
void respond_json(int client_fd, const char *json) {
    send_response(client_fd, "200 OK", "application/json", json);
}

// Respond with HTML wrapper
void respond_html(int client_fd, const char *page_json) {
    char html[8192];
    snprintf(html, sizeof(html),
        "<!DOCTYPE html>\n"
        "<html lang=\"en\">\n"
        "<head>\n"
        "    <meta charset=\"UTF-8\">\n"
        "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">\n"
        "    <title>C Recipe - Switchback</title>\n"
        "    <script>window.initialPage = %s;</script>\n"
        "</head>\n"
        "<body>\n"
        "    <div data-swbk-app>\n"
        "        <div style=\"padding: 2rem; text-align: center; background: #0a0e0a; color: #00ff41;\">\n"
        "            <p>Loading Switchback app...</p>\n"
        "        </div>\n"
        "    </div>\n"
        "    <script type=\"module\" src=\"/dist/app.js\"></script>\n"
        "</body>\n"
        "</html>",
        page_json);

    send_response(client_fd, "200 OK", "text/html", html);
}

// Parse HTTP request
int parse_request(const char *buffer, size_t buffer_len, HttpRequest *req) {
    memset(req, 0, sizeof(HttpRequest));

    // Parse request line
    if (sscanf(buffer, "%15s %255s", req->method, req->uri) != 2) {
        return -1;
    }

    // Check for X-Switchback header
    const char *header_pos = buffer;
    while ((header_pos = strchr(header_pos, '\n')) != NULL) {
        header_pos++;
        if (*header_pos == '\r' || *header_pos == '\n') {
            // End of headers
            header_pos += (*header_pos == '\r') ? 2 : 1;
            req->body = (char *)header_pos;
            break;
        }

        if (strncasecmp(header_pos, "X-Switchback:", 13) == 0 ||
            strncasecmp(header_pos, "x-switchback:", 13) == 0) {
            req->is_switchback = 1;
        }

        if (strncasecmp(header_pos, "Content-Length:", 15) == 0) {
            req->content_length = atoi(header_pos + 15);
        }
    }

    return 0;
}

// Signal handler for graceful shutdown
void signal_handler(int signum) {
    (void)signum; // Unused parameter
    printf("\n💚 Shutting down gracefully...\n");
    server_running = 0;

    // Close server socket to unblock accept()
    if (server_fd_global >= 0) {
        shutdown(server_fd_global, SHUT_RDWR);
        close(server_fd_global);
    }
}

// Handle client connection
void *handle_connection(void *arg) {
    int client_fd = *(int *)arg;
    free(arg);

    char buffer[BUFFER_SIZE];
    ssize_t bytes_read = read(client_fd, buffer, sizeof(buffer) - 1);

    if (bytes_read <= 0) {
        close(client_fd);
        return NULL;
    }

    buffer[bytes_read] = '\0';

    HttpRequest req;
    if (parse_request(buffer, bytes_read, &req) < 0) {
        send_response(client_fd, "400 Bad Request", "text/plain", "Bad Request");
        close(client_fd);
        return NULL;
    }

    // Serve static files
    if (strncmp(req.uri, "/dist/", 6) == 0) {
        serve_static(client_fd, req.uri);
        close(client_fd);
        return NULL;
    }

    // Handle API routes
    if (strncmp(req.uri, "/api/", 5) == 0) {
        char *json = handle_api_route(&req);
        respond_json(client_fd, json);
        free(json);
        close(client_fd);
        return NULL;
    }

    // Handle page routes
    char *json = handle_page_route(&req);

    if (req.is_switchback) {
        respond_json(client_fd, json);
    } else {
        respond_html(client_fd, json);
    }

    free(json);
    close(client_fd);
    return NULL;
}

// Initialize some sample todos
void init_sample_todos() {
    pthread_mutex_lock(&todos_mutex);

    const char *samples[] = {
        "Try optimistic updates!",
        "Notice the instant UI feedback",
        "Like this todo to see it in action"
    };

    for (int i = 0; i < 3; i++) {
        Todo *todo = malloc(sizeof(Todo));
        todo->id = next_todo_id++;
        strncpy(todo->text, samples[i], sizeof(todo->text) - 1);
        todo->text[sizeof(todo->text) - 1] = '\0';
        todo->completed = 0;
        todo->likes = i; // Give first one some likes
        todo->next = todos_head;
        todos_head = todo;
        total_likes += i;
    }

    pthread_mutex_unlock(&todos_mutex);
}

int main() {
    int server_fd;
    struct sockaddr_in address;
    int opt = 1;

    // Initialize sample todos
    init_sample_todos();

    // Install signal handlers for graceful shutdown
    signal(SIGTERM, signal_handler);
    signal(SIGINT, signal_handler);

    // Create socket
    if ((server_fd = socket(AF_INET, SOCK_STREAM, 0)) == 0) {
        perror("socket failed");
        exit(EXIT_FAILURE);
    }

    server_fd_global = server_fd;

    // Set socket options
    if (setsockopt(server_fd, SOL_SOCKET, SO_REUSEADDR | SO_REUSEPORT, &opt, sizeof(opt))) {
        perror("setsockopt");
        exit(EXIT_FAILURE);
    }

    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(PORT);

    // Bind socket
    if (bind(server_fd, (struct sockaddr *)&address, sizeof(address)) < 0) {
        perror("bind failed");
        exit(EXIT_FAILURE);
    }

    // Listen
    if (listen(server_fd, 10) < 0) {
        perror("listen");
        exit(EXIT_FAILURE);
    }

    printf("💚 C server listening on http://0.0.0.0:%d\n", PORT);
    printf("   Try optimistic updates at /todos!\n");

    // Accept connections
    while (server_running) {
        struct sockaddr_in client_addr;
        socklen_t client_len = sizeof(client_addr);

        int *client_fd = malloc(sizeof(int));
        *client_fd = accept(server_fd, (struct sockaddr *)&client_addr, &client_len);

        if (*client_fd < 0) {
            free(client_fd);
            if (!server_running) {
                // Server is shutting down, break out of loop
                break;
            }
            perror("accept");
            continue;
        }

        // Spawn thread to handle connection
        pthread_t thread;
        if (pthread_create(&thread, NULL, handle_connection, client_fd) != 0) {
            perror("pthread_create");
            close(*client_fd);
            free(client_fd);
            continue;
        }

        pthread_detach(thread);
    }

    // Clean shutdown - free todos
    pthread_mutex_lock(&todos_mutex);
    Todo *current = todos_head;
    while (current) {
        Todo *next = current->next;
        free(current);
        current = next;
    }
    pthread_mutex_unlock(&todos_mutex);

    // Clean shutdown
    if (server_fd >= 0 && server_fd == server_fd_global) {
        close(server_fd);
    }
    printf("💚 Server stopped\n");
    return 0;
}
