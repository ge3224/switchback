# Zig Demo - Switchback Integration

A minimal example showing how to integrate Switchback with Zig's standard library HTTP server. Blazingly fast with zero runtime overhead!

## What's Included

- **server.zig** - Zig HTTP server with routing and `X-Switchback` header detection
- **app.ts** - Client-side Switchback app (TypeScript)
- **vite.config.ts** - Bundles app.ts + Switchback into single JS file
- **Docker setup** - Multi-stage build with Zig compiler

## Try It Out

Want to see Zig's performance without installing Zig locally?

```bash
cd examples/demos/zig
docker-compose up
```

Open http://localhost:8000

## Running Natively

To run this demo with a local Zig installation, see the [Zig download page](https://ziglang.org/download/).

## How It Works

1. **Initial Request**: Browser requests `/` → Zig returns full HTML with `window.initialPage`
2. **Switchback Request**: User clicks link → Switchback adds `X-Switchback` header → Zig returns JSON
3. **Client Rendering**: Switchback receives JSON, swaps page component, updates URL

```zig
// Detect Switchback request
const is_switchback = request.head.headers.contains("x-switchback");

// Build JSON response
try std.json.stringify(.{
    .component = "Home",
    .props = .{ .title = "Welcome" },
    .url = "/",
}, .{}, writer);

// Return JSON or HTML
if (is_switchback) {
    try respondJson(allocator, &request.response, json);
} else {
    try respondHtml(allocator, &request.response, json);
}
```

## Key Features Demonstrated

- ✅ Multi-threaded HTTP server with `std.net.Server`
- ✅ Static file serving for `/dist/app.js`
- ✅ Manual routing with string matching
- ✅ JSON serialization with `std.json.stringify`
- ✅ X-Switchback header detection
- ✅ Zero external dependencies (Zig stdlib only)
- ✅ Compile-time safety guarantees

## File Structure

```
zig/
├── server.zig         # Backend HTTP server
├── app.ts             # Frontend Switchback app
├── vite.config.ts     # Vite bundler config
├── package.json       # Build scripts
├── Dockerfile         # Docker image
├── docker-compose.yml # Docker setup
└── README.md          # This file
```

## Extending This Example

### Add a New Route

In `server.zig`:

```zig
} else if (mem.eql(u8, uri, "/posts")) {
    try std.json.stringify(.{
        .component = "Posts/Index",
        .props = .{ .posts = posts },
        .url = "/posts",
    }, .{}, writer);
}
```

In `app.ts`:

```typescript
const pages = {
  // ... existing pages
  'Posts/Index': (props) => Layout(
    h('div', {},
      h('h1', {}, 'Posts'),
      // ... your component
    )
  ),
};
```

### Connect to a Database

You can use Zig libraries like [pg.zig](https://github.com/karlseguin/pg.zig) for PostgreSQL:

```zig
const pg = @import("pg");

var pool = try pg.Pool.init(allocator, .{
    .host = "localhost",
    .port = 5432,
    .database = "mydb",
});
defer pool.deinit();

const result = try pool.query("SELECT * FROM users", .{});
```

### Use a Router Library

For more complex routing, consider:
- [httpz](https://github.com/karlseguin/http.zig) - High-performance HTTP server with routing
- [zap](https://github.com/zigzap/zap) - Fast HTTP server built on facil.io

## Troubleshooting

**App not loading?**
- Make sure you've run `pnpm build` in the demo directory
- Check that `dist/app.js` exists
- Check browser console for import errors

**Compilation errors?**
- This demo requires Zig 0.13.0 exactly
- Run `zig version` to verify
- Download from https://ziglang.org/download/

**Routes not working?**
- Make sure you're using `data-swbk` attribute on links for SPA navigation
- Check server output for request logs

**Docker issues?**
- Make sure port 8000 is not in use: `lsof -i :8000`
- Rebuild with `docker-compose build --no-cache`
- Zig download may be slow - be patient during first build

**Build issues?**
- Make sure parent dependencies are installed: `pnpm install --dir ../../../`
- TypeScript errors? Check that tsconfig.json exists in project root

## Performance Notes

Zig's HTTP server is extremely fast:
- Zero-cost abstractions
- Compile-time optimizations
- No garbage collection overhead
- Minimal memory footprint (~10MB runtime)

For production use, consider:
- Enabling `-O ReleaseFast` or `-O ReleaseSafe` optimizations
- Using connection pooling for database queries
- Adding rate limiting and request validation
- Implementing graceful shutdown handling

## Why Zig?

This demo demonstrates Switchback with a systems programming language:
- **Performance**: Comparable to C/C++ but with memory safety
- **Simplicity**: No complex build systems or package managers
- **Portability**: Cross-compile to any platform
- **Learning**: Great introduction to low-level HTTP servers

Perfect for high-performance APIs, microservices, or learning systems programming!
