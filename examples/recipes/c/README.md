# C Recipe - Switchback Integration

A minimal example showing how to integrate Switchback with a classic C HTTP server. Demonstrates **optimistic updates** for instant UI feedback!

## What's Included

- **server.c** - C HTTP server with Switchback + optimistic updates API
- **app.ts** - Client-side Switchback app with Matrix-style green theme
- **vite.config.ts** - Bundles app.ts + Switchback into single JS file
- **Docker setup** - Multi-stage build with GCC compiler

## What are Optimistic Updates?

Optimistic updates are a UI pattern where you update the interface **immediately** when the user performs an action, without waiting for the server to respond.

### Benefits:
- ⚡ Instant feedback - feels lightning fast
- 🎯 Better UX - no waiting for network requests
- 🔄 Graceful rollback - revert if server fails
- 📱 Essential for mobile apps with spotty connections

### How It Works in This Recipe:
1. User adds a todo
2. UI updates **immediately** (optimistic todo shown with dashed border)
3. Request sent to C backend
4. Server processes and responds (with artificial 300ms delay for demo)
5. UI replaces optimistic todo with confirmed one (solid border)

If the server fails, the optimistic update is rolled back and an error is shown.

## Running Locally

### Prerequisites

- GCC compiler (or clang)
- Node.js 20+ with pnpm
- POSIX-compatible system (Linux, macOS, BSD)

### Build and Run

Build the bundled client app:

```bash
cd examples/recipes/c

# Install dependencies (uses parent's node_modules for vite/typescript)
pnpm install --dir ../../../

# Build app.ts + Switchback into dist/app.js
pnpm build
```

Build and run the C server:

```bash
# Compile C server
gcc -o server server.c -pthread -O2 -Wall

# Run server
./server
```

Open http://localhost:8000

**For development with auto-rebuild:**

```bash
# Terminal 1: Watch and rebuild client on changes
pnpm dev

# Terminal 2: Run C server (recompile manually when server.c changes)
gcc -o server server.c -pthread -O2 -Wall && ./server
```

### Option 2: Docker (Recommended)

Docker will automatically build everything:

```bash
# From examples/recipes/c directory
docker-compose up

# Or build first, then run
docker-compose build
docker-compose up
```

Open http://localhost:8000

**Note:** The Dockerfile uses a multi-stage build:
1. **JS builder stage**: Bundles app.ts with Switchback source into single JS file
2. **C builder stage**: Compiles server.c with GCC
3. **Runtime stage**: Minimal Alpine image with compiled binary

## Try the Optimistic Updates

1. Navigate to the **Todos** page
2. Add a new todo - watch it appear **instantly** with a dashed border
3. Wait ~300ms - the border becomes solid when the server confirms
4. Click the **Like** button - it updates immediately
5. Delete a todo - it disappears instantly

The UI feels instant because it doesn't wait for the server!

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
├── app.ts             # Frontend Switchback app with optimistic updates
├── vite.config.ts     # Vite bundler config
├── package.json       # Build scripts
├── Dockerfile         # Docker image
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
- Make sure you've run `pnpm build` in the recipe directory
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

This recipe demonstrates Switchback with a systems programming language:
- **Performance**: Native compiled code with minimal overhead
- **Portability**: Runs on virtually any platform
- **Control**: Direct memory management and system access
- **Learning**: Great introduction to low-level HTTP servers
- **Legacy**: Can integrate with existing C codebases

Perfect for high-performance APIs, embedded systems, or learning systems programming!

## Comparison with Other Recipes

- **PHP Recipe**: Shows basic navigation with a scripting language
- **Zig Recipe**: Shows modern systems programming with compile-time safety and form handling
- **C Recipe**: Shows classic systems programming with **optimistic updates** for instant UX

All three demonstrate different Switchback features with different language paradigms and trade-offs.

## Learn More About Optimistic Updates

- [Optimistic UI Patterns](https://www.apollographql.com/docs/react/performance/optimistic-ui/) - Apollo GraphQL docs
- [The Perils of Rehydration](https://www.joshwcomeau.com/react/the-perils-of-rehydration/) - Josh Comeau
- [Building Resilient Frontend Architecture](https://resilientwebdesign.com/) - Jeremy Keith

Optimistic updates are a key pattern for building fast, responsive web applications that feel native!
