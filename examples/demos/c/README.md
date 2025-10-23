# C Demo - Optimistic Updates with C Backend

A single-page example showing how Switchback enables **optimistic updates** with a pure C HTTP server. The UI updates instantly before the server responds, providing a native app-like experience.

## What This Demo Shows

### Optimistic Updates Pattern

**Backend (server.c):**
- C server handles API routes and serves a single page
- Returns `{component, props, url}` JSON with both stats and todos
- RESTful API for todos: POST, DELETE, and PATCH operations
- Artificial delays (300ms for adds, 200ms for likes) make optimistic behavior visible

**Frontend (app.ts):**
- Single-page app with all features on one screen
- UI updates **instantly** when user takes action
- Visual feedback: dashed border while pending, solid when confirmed
- Graceful rollback on errors with custom styled modals
- No navigation - everything on one page

### Key UX Features

The demo showcases modern web app patterns:
- **Instant feedback**: UI updates immediately before server responds
- **Visual states**: Dashed border while pending, solid when confirmed
- **Custom modals**: Styled confirmation and error dialogs matching the theme
- **Graceful errors**: Automatic rollback with user-friendly error messages
- **Compact layout**: Everything above the fold for easy interaction

### Why It Works for C

Switchback's requirements are minimal:
- Return structured JSON
- Detect one header
- Serve static files

This maps cleanly to C's strengths (direct HTTP handling, simple JSON formatting) without requiring framework complexity.

## What's Included

- **server.c** - Multi-threaded C HTTP server with todos API
- **app.ts** - Single-page Switchback app with optimistic updates and Matrix-style green theme
- **Dockerfile** - Multi-stage build
- **docker-compose.yml** - Docker setup

## What are Optimistic Updates?

Optimistic updates are a UI pattern where you update the interface **immediately** when the user performs an action, without waiting for the server to respond.

### Benefits:
- ⚡ Instant feedback - feels lightning fast
- 🎯 Better UX - no waiting for network requests
- 🔄 Graceful rollback - revert if server fails
- 📱 Essential for mobile apps with spotty connections

### How It Works in This Demo:
1. User adds a todo
2. UI updates **immediately** (optimistic todo shown with dashed border)
3. Request sent to C backend
4. Server processes and responds (with artificial 300ms delay for demo)
5. UI replaces optimistic todo with confirmed one (solid border)

If the server fails, the optimistic update is rolled back and an error is shown.

## Try the Optimistic Updates

The app is a single page with everything visible:

1. **Add a todo** - watch it appear **instantly** with a dashed border
2. Wait ~300ms - the border becomes solid when the server confirms
3. Click the **Like** button - it updates immediately before the server responds
4. Click **Delete** - custom styled modal appears for confirmation
5. Confirm delete - todo disappears instantly

The UI feels instant because it doesn't wait for the server!

## Try It Out

The easiest way to run this demo is with Docker:

```bash
cd examples/demos/c
docker-compose up
```

Open http://localhost:8000

## Running Locally

If you want to develop locally without Docker:

1. **Build the frontend bundle:**
   ```bash
   cd examples/demos/c
   pnpm install  # If you haven't installed root dependencies
   pnpm build    # Creates dist/app.js
   ```

2. **Compile and run the C server:**
   ```bash
   gcc -o server server.c -pthread -O2 -Wall
   ./server
   ```

Most systems have GCC or Clang pre-installed. Check with `gcc --version`.

## API Endpoints

The C server provides these API endpoints for optimistic updates:

### `POST /api/todos`
Add a new todo (with 300ms artificial delay)

```bash
curl -X POST http://localhost:8000/api/todos \
  -H "Content-Type: application/json" \
  -d '{"text":"My new todo"}'
```

Response:
```json
{"todo":{"id":4,"text":"My new todo","completed":false,"likes":0}}
```

### `POST /api/todos/:id/like`
Toggle like on a todo (with 200ms artificial delay)

```bash
curl -X POST http://localhost:8000/api/todos/1/like \
  -H "Content-Type: application/json" \
  -d '{"liked":true}'
```

Response:
```json
{"likes":1}
```

### `DELETE /api/todos/:id`
Delete a todo

```bash
curl -X DELETE http://localhost:8000/api/todos/1
```

Response:
```json
{"success":true}
```

## Key Features Demonstrated

- ✅ **Optimistic UI updates** - instant feedback before server confirms
- ✅ **Custom styled modals** - no browser default dialogs
- ✅ **Compact single-page layout** - everything above the fold
- ✅ Multi-threaded HTTP server with pthread
- ✅ RESTful API with JSON parsing
- ✅ In-memory todo storage with linked list
- ✅ Thread-safe operations with mutex
- ✅ Graceful error handling and rollback
- ✅ Artificial delays to showcase optimistic UX
- ✅ Matrix-style green terminal theme

## File Structure

```
c/
├── server.c           # Backend HTTP server with todos API
├── app.ts             # Frontend single-page app with optimistic updates
├── Dockerfile         # Docker build
├── docker-compose.yml # Docker setup
└── README.md          # This file
```

## Extending This Example

### Add a New Field to Todos

In `server.c`, update the `Todo` struct:

```c
typedef struct Todo {
    int id;
    char text[256];
    int completed;
    int likes;
    char priority[16];  // NEW: priority field
    struct Todo *next;
} Todo;
```

Update `build_todos_json` to include the new field in JSON output.

In `app.ts`, update the optimistic todo creation:

```typescript
state.optimisticTodo = {
  id: optimisticId,
  text,
  completed: false,
  likes: 0,
  priority: 'medium'  // NEW
};
```

### Add Persistence with SQLite

You can add SQLite for persistent storage:

```c
#include <sqlite3.h>

sqlite3 *db;
int rc = sqlite3_open("todos.db", &db);

if (rc == SQLITE_OK) {
    sqlite3_exec(db,
        "CREATE TABLE IF NOT EXISTS todos (id INTEGER PRIMARY KEY, text TEXT, likes INTEGER)",
        NULL, NULL, NULL);
}

// Save todo to database
char sql[512];
snprintf(sql, sizeof(sql), "INSERT INTO todos (text, likes) VALUES ('%s', %d)",
         todo->text, todo->likes);
sqlite3_exec(db, sql, NULL, NULL, NULL);
```

### Adjust Network Delay

The server has artificial delays to demonstrate optimistic updates. Adjust these in `server.c`:

```c
// In handle_api_route()
usleep(300000); // 300ms delay for adding todos
usleep(200000); // 200ms delay for likes
```

For production, remove these delays entirely.

## Troubleshooting

**App not loading?**
- Make sure you've run `pnpm build` in the demo directory
- Check that `dist/app.js` exists
- Check browser console for import errors

**Compilation errors?**
- Make sure GCC is installed: `gcc --version`
- On macOS, install Xcode Command Line Tools: `xcode-select --install`
- On Ubuntu/Debian: `sudo apt-get install build-essential`
- Try using clang instead: `clang -o server server.c -pthread -O2 -Wall`

**Optimistic updates not working?**
- Open browser DevTools Network tab
- Check that API requests are being sent to `/api/todos`
- Check server logs for errors
- Verify the X-Switchback header is present

**Docker issues?**
- Make sure port 8000 is not in use: `lsof -i :8000` or `netstat -an | grep 8000`
- Rebuild with `docker-compose build --no-cache`

**Build issues?**
- Make sure parent dependencies are installed: `pnpm install --dir ../../../`
- TypeScript errors? Check that tsconfig.json exists in project root

**Memory leaks?**
- Use valgrind to check for memory leaks: `valgrind --leak-check=full ./server`
- Make sure all malloc'd memory is freed

## Performance Notes

C's HTTP server is extremely fast:
- Direct memory manipulation
- Minimal overhead
- Native compiled code
- Low memory footprint (~5MB runtime)

The artificial delays (300ms for adds, 200ms for likes) are intentional to demonstrate optimistic updates. Remove them for production use.

## Security Considerations

This is a **demo application** and should not be used in production without additional hardening:

- ⚠️ No input validation or sanitization
- ⚠️ Simple JSON parsing (not production-ready)
- ⚠️ No authentication or authorization
- ⚠️ Fixed-size buffers (potential overflow)
- ⚠️ No HTTPS/TLS support
- ⚠️ In-memory storage (data lost on restart)

For production, add:
- Input validation and sanitization
- Proper JSON parsing library (jsmn, cJSON, etc.)
- HTTPS with OpenSSL or mbedTLS
- Authentication and session management
- Rate limiting and request throttling
- Persistent storage (SQLite, PostgreSQL, etc.)
- Proper error handling and logging

## Why C?

This demodemonstrates Switchback with a systems programming language:
- **Performance**: Native compiled code with minimal overhead
- **Portability**: Runs on virtually any platform
- **Control**: Direct memory management and system access
- **Learning**: Great introduction to low-level HTTP servers
- **Legacy**: Can integrate with existing C codebases

Perfect for high-performance APIs, embedded systems, or learning systems programming!

## Comparison with Other Demos

- **PHP Demo**: Shows basic navigation with a scripting language
- **Zig Demo**: Shows modern systems programming with compile-time safety and form handling
- **Rust Demo**: Shows image processing with optimistic updates
- **C Demo**: Single-page app focused on **optimistic updates** with custom modals and compact layout

Each demo showcases different Switchback features with different language paradigms and UX patterns.

## Learn More About Optimistic Updates

- [Optimistic UI Patterns](https://www.apollographql.com/docs/react/performance/optimistic-ui/) - Apollo GraphQL docs
- [The Perils of Rehydration](https://www.joshwcomeau.com/react/the-perils-of-rehydration/) - Josh Comeau
- [Building Resilient Frontend Architecture](https://resilientwebdesign.com/) - Jeremy Keith

Optimistic updates are a key pattern for building fast, responsive web applications that feel native!
