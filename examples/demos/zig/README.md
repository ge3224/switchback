# Zig Demo - Form Handling with POST Requests

**Blazingly fast Zig backend + Switchback = Server-side form handling without page reloads**

This demo showcases Switchback's **form handling with POST requests** using a minimal multi-threaded Zig HTTP server. It's a single-page application that demonstrates the complete form submission lifecycle without any page reloads.

## What Makes This Demo Special

A focused demonstration of form handling fundamentals:

- ✅ **Form handling with POST requests** - Submit forms without page reloads
- ✅ **Server-side state management** - Track submissions server-side (in-memory counter)
- ✅ **Instant feedback** - Success page renders immediately after submission
- ✅ **Zero framework dependencies** - Pure Zig stdlib + vanilla JS

## Try It Out

Run the full stack (Zig server + frontend build) in Docker:

```bash
cd examples/demos/zig
docker-compose up
```

Open http://localhost:8000

**That's it!** Docker Compose handles the Zig compilation, TypeScript bundling, and server startup automatically.

## How It Works

### The Core Pattern

The server detects the `X-Switchback` header and responds differently:

```zig
var is_switchback = false;
while (lines.next()) |line| {
    if (mem.startsWith(u8, line, "X-Switchback:")) {
        is_switchback = true;
    }
}

// Dual response strategy
if (is_switchback) {
    try respondJson(connection.stream, json);  // JSON for form submission
} else {
    try respondHtml(allocator, connection.stream, json);  // HTML for first load
}
```

### The Request Flow

**1. Initial Load** (`GET /` without header)
```
Browser → Zig Server
         ← Full HTML with <script>window.initialPage = {...}</script>
Client-side app hydrates with form and stats (no loading state!)
```

**2. Form Submission** (`POST /` with `X-Switchback: true`)
```
User submits <form data-swbk>
Client → Zig Server (with X-Switchback header + FormData)
Zig processes form, increments counter
        ← JSON: {"component":"Success","props":{...}}
Client renders success page (no reload!)
```

### The Architecture

**Backend (server.zig):**
- Single route: `GET /` and `POST /`
- State management: `submission_count` in memory
- Form parsing: Extracts name and email from multipart FormData
- Returns component name + props as JSON

**Frontend (app.ts):**
- Two components: `Home` (with form) and `Success`
- Intercepts form with `data-swbk` attribute
- Adds `X-Switchback` header to POST requests
- Renders success component after submission

## Key Files

```
zig/
├── server.zig         # Multi-threaded HTTP server
│                      # Route: / (GET/POST)
│                      # Dual response: HTML or JSON based on header
│
├── app.ts             # Switchback client app
│                      # Components: Home (with form), Success
│                      # Handles form interception
│
├── Dockerfile         # Multi-stage: Deno + Zig build
├── docker-compose.yml # One command to run everything
└── README.md          # You are here
```

## Extending This Demo

### Add More Form Fields

Update the form in `app.ts`:
```typescript
h('label', { for: 'message' }, '> Message:'),
h('textarea', {
  id: 'message',
  name: 'message',
  placeholder: 'Your message...',
  required: true
})
```

Parse it in `server.zig`:
```zig
var form_message: []const u8 = "";
// ... in the parsing loop:
else if (mem.eql(u8, current_field, "message")) {
    form_message = try allocator.dupe(u8, line);
}
```

### Store Submissions

Instead of just counting, store the actual submissions:
```zig
const Submission = struct {
    name: []const u8,
    email: []const u8,
};

var submissions = std.ArrayList(Submission).init(allocator);

// After form submission:
try submissions.append(.{
    .name = try allocator.dupe(u8, form_name),
    .email = try allocator.dupe(u8, form_email),
});
```

Then return them in the Home component props to display recent submissions.

### Connect to a Database

Use Zig database libraries like [pg.zig](https://github.com/karlseguin/pg.zig):

```zig
const pg = @import("pg");

var pool = try pg.Pool.init(allocator, .{
    .connect = .{ .host = "localhost", .port = 5432 },
    .auth = .{ .username = "user", .database = "db", .password = "pass" },
});
defer pool.deinit();

// In your route handler
const result = try pool.query("SELECT name, email FROM users", .{});
defer result.deinit();

// Build JSON with real data
json = try std.fmt.allocPrint(allocator,
    \\{{"component":"Users","props":{{"users":[...]}},"url":"/users"}}
, .{});
```

## Running Natively (Without Docker)

**Prerequisites:**
- [Zig 0.13](https://ziglang.org/download/) for the server
- [Deno](https://deno.land/) for frontend build (or use Docker)

**Steps:**
```bash
cd examples/demos/zig

# 1. Build frontend with Deno
mkdir -p dist
deno run -A npm:esbuild app.ts --bundle --outfile=dist/app.js --format=esm --platform=browser

# 2. Compile Zig server
zig build-exe server.zig

# 3. Run server
./server
```

Open http://localhost:8000

## Performance Notes

Zig's HTTP server is **extremely fast**:
- Zero-cost abstractions (no runtime overhead)
- Compile-time optimizations
- No garbage collection pauses
- Minimal memory footprint (~10MB)
- Multi-threaded connection handling

For production:
```bash
# Build with optimizations
zig build-exe server.zig -O ReleaseFast
```

## What This Shows About Switchback

This demo is intentionally minimal to show what Switchback actually requires:

- 🎯 **Simple backend** - Return JSON and detect one header
- ⚡ **No page reloads** - Forms submit and render new content instantly
- 🔧 **Server decides** - You control which component renders with what props
- 📦 **No framework needed** - Just vanilla JS and your backend language
- 🚀 **Works without JS** - Forms still POST normally if JavaScript is disabled

Zig, C, PHP, Rust—doesn't matter. If you can return JSON, you can use Switchback. This demo shows the pattern at its simplest: detect a header, return the right response format, done.
