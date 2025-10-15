# Deno Recipe - Switchback Integration

A real-time dashboard showcasing **Server-Sent Events (SSE)** for live data streaming from a Deno backend to a Switchback SPA.

## 🎯 What This Demonstrates

This recipe shows the **powerful real-time interplay between Switchback and Deno**:

- **Server-Sent Events (SSE)**: Unidirectional push updates from server to client
- **Real-Time Metrics**: Live CPU, memory, connections streaming every second
- **Activity Feed**: Instant updates when events occur on the server
- **No Polling**: Browser doesn't repeatedly ask for updates - server pushes them
- **Deno Native**: Built-in TypeScript, modern APIs, zero config needed

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────┐
│  Switchback (Frontend SPA)                      │
│  ├─ app.visit() for client-side routing         │
│  ├─ EventSource API for SSE connections         │
│  ├─ Real-time UI updates on data push           │
│  └─ Live charts and activity feed               │
└─────────────────┬───────────────────────────────┘
                  │ SSE Streams
┌─────────────────▼───────────────────────────────┐
│  Deno Backend (SSE Server)                      │
│  ├─ ReadableStream for SSE connections          │
│  ├─ Real-time metrics broadcast (every 1s)      │
│  ├─ Activity log streaming on events            │
│  └─ RESTful fallback endpoints                  │
└─────────────────────────────────────────────────┘
```

## 🔗 What Are Server-Sent Events?

**Server-Sent Events (SSE)** is a web standard for **pushing updates from server to browser** over HTTP.

### Key Differences from Other Approaches

| Feature | SSE | WebSockets | Polling |
|---------|-----|------------|---------|
| **Direction** | Server → Client (one-way) | Bidirectional | Client asks repeatedly |
| **Protocol** | HTTP/HTTPS | Custom (ws://) | HTTP/HTTPS |
| **Auto-Reconnect** | Built-in | Manual | N/A |
| **Complexity** | Simple | Complex | Simple but wasteful |
| **Use Case** | Live updates, feeds | Chat, gaming | Legacy compatibility |

### Why SSE for This Recipe?

- ✅ **Perfect for dashboards**: Server pushes metrics, client displays them
- ✅ **Simpler than WebSockets**: No need for bidirectional communication
- ✅ **Native browser support**: EventSource API built into all modern browsers
- ✅ **Automatic reconnection**: Browser handles reconnects if connection drops
- ✅ **HTTP-friendly**: Works through proxies, load balancers, CDNs

## 🚀 Running the Demo

### Prerequisites

- [Deno 2.1+](https://deno.land/) - **That's it!** No Node.js required 🎉
- Or just use Docker

### Option 1: Pure Deno Build (Recommended)

Build and run with **pure Deno** - no Node.js, no npm, no pnpm needed:

```bash
cd examples/recipes/deno

# Build app.ts + Switchback into dist/app.js using Deno + esbuild
deno task build

# Run server
deno task serve
```

Open http://localhost:8000

**For development with auto-reload:**

```bash
# Terminal 1: Build once (or rebuild when you change app.ts)
deno task build

# Terminal 2: Run Deno server with auto-restart on server changes
deno task dev
```

### Option 2: Node.js Build (Legacy)

If you prefer using Node.js tools:

```bash
# Install dependencies (uses parent's node_modules for vite/typescript)
pnpm install --dir ../../../

# Build app.ts + Switchback into dist/app.js
pnpm build

# Run server
deno run --allow-net --allow-read server.ts
```

### Option 3: Docker (Easiest)

Docker will automatically build everything using **pure Deno** (no Node.js in the container):

```bash
# From examples/recipes/deno directory
docker-compose up

# Or build first, then run
docker-compose build
docker-compose up
```

Open http://localhost:8000

**Note:** The Dockerfile uses a multi-stage build with **100% Deno**:
1. **Builder stage**: Uses Deno to bundle app.ts with Switchback (via esbuild from npm)
2. **Runtime stage**: Minimal Deno image with compiled JS bundle
3. **No Node.js required!** Everything runs through Deno's npm compatibility layer

## 📊 Try the Real-Time Features

### Dashboard Page

1. Watch the **CPU and Memory metrics** update every second
2. See the **mini-charts** grow as data streams in
3. Observe the **Active Connections** counter change as you open/close tabs
4. Check the **Stream Status** showing live SSE connections

### Activity Feed

1. Navigate to the **Activity Log** page
2. Watch new activities appear **instantly** without refreshing
3. Try the **"Add Custom Activity"** form
4. Submit an activity and see it appear in real-time

### Multi-Tab Test

1. Open the dashboard in **multiple browser tabs**
2. All tabs receive the **same real-time updates simultaneously**
3. Add an activity in one tab - see it appear in **all tabs instantly**

## 🔧 How SSE Works in This Recipe

### Server Side (Deno)

```typescript
// Create SSE stream with ReadableStream
function createSSEStream(clientSet: Set<ReadableStreamDefaultController>) {
  return new ReadableStream({
    start(controller) {
      clientSet.add(controller);
      // Keep connection alive
      controller.enqueue(new TextEncoder().encode(': heartbeat\n\n'));
    },
    cancel() {
      clientSet.delete(controller); // Clean up on disconnect
    }
  });
}

// Broadcast to all connected clients
function broadcastMetrics(metrics: Metrics) {
  const data = `data: ${JSON.stringify(metrics)}\n\n`;

  metricsClients.forEach(controller => {
    controller.enqueue(new TextEncoder().encode(data));
  });
}

// SSE endpoint
if (path === '/api/metrics/stream') {
  const stream = createSSEStream(metricsClients);

  return new Response(stream, {
    headers: {
      'Content-Type': 'text/event-stream',
      'Cache-Control': 'no-cache',
      'Connection': 'keep-alive',
    }
  });
}
```

### Client Side (Switchback)

```typescript
// Connect to SSE stream
metricsEventSource = new EventSource('/api/metrics/stream');

// Handle incoming messages
metricsEventSource.onmessage = (event) => {
  const metrics = JSON.parse(event.data);
  state.metrics = metrics;

  // Add to history for charting
  state.metricsHistory.push({
    timestamp: Date.now(),
    cpu: metrics.cpu,
    memory: metrics.memory
  });

  // Manually update DOM (avoid full page reload for smooth updates)
  updateMetricsUI();
};

// Handle errors and reconnection
metricsEventSource.onerror = () => {
  console.error('Stream disconnected');
  metricsEventSource?.close();
};
```

## 📡 API Endpoints

### SSE Endpoints (Real-Time Streaming)

#### `GET /api/metrics/stream`
Stream real-time system metrics (updates every 1 second)

```bash
curl -N http://localhost:8000/api/metrics/stream
```

Response (SSE format):
```
data: {"timestamp":"2024-01-15T10:30:00Z","cpu":45.2,"memory":62.1,"activeConnections":3,"totalRequests":127,"uptime":3600}

data: {"timestamp":"2024-01-15T10:30:01Z","cpu":46.8,"memory":62.3,"activeConnections":3,"totalRequests":128,"uptime":3601}
```

#### `GET /api/activity/stream`
Stream real-time activity log updates (as events occur)

```bash
curl -N http://localhost:8000/api/activity/stream
```

Response (SSE format):
```
data: {"id":42,"timestamp":"2024-01-15T10:30:05Z","type":"success","message":"API request completed","details":"Response time: 125ms"}
```

### REST Endpoints (Fallback)

#### `GET /api/metrics`
Get current metrics snapshot

```bash
curl http://localhost:8000/api/metrics
```

Response:
```json
{
  "timestamp": "2024-01-15T10:30:00Z",
  "cpu": 45.2,
  "memory": 62.1,
  "activeConnections": 3,
  "totalRequests": 127,
  "uptime": 3600
}
```

#### `GET /api/activity`
Get full activity log

```bash
curl http://localhost:8000/api/activity
```

Response:
```json
[
  {
    "id": 1,
    "timestamp": "2024-01-15T10:30:00Z",
    "type": "success",
    "message": "Server started",
    "details": "Deno server initialized on port 8000"
  }
]
```

#### `POST /api/activity`
Add a custom activity

```bash
curl -X POST http://localhost:8000/api/activity \
  -H "Content-Type: application/json" \
  -d '{
    "type": "info",
    "message": "Custom event",
    "details": "Triggered from curl"
  }'
```

## 🦕 Why Deno?

### Advantages Over Node.js

| Feature | Deno | Node.js |
|---------|------|---------|
| **TypeScript** | Built-in, zero config | Requires ts-node or compilation |
| **Security** | Explicit permissions | Full access by default |
| **Standard Library** | Comprehensive, stable | Fragmented in npm |
| **Module System** | ESM only, URL/npm imports | CommonJS + ESM |
| **Tooling** | Bundled (fmt, lint, test, bundle) | Separate packages |
| **Dependency Management** | No node_modules (Deno 2.0+) | npm/yarn/pnpm needed |
| **npm Compatibility** | Direct `npm:` imports (Deno 2.0+) | Native |

### Perfect For

- ✅ **Modern web APIs**: Built-in fetch, WebSocket, SSE support
- ✅ **TypeScript-first**: No build step for server code
- ✅ **Security**: Explicit --allow-net, --allow-read flags
- ✅ **Simplicity**: Single executable, minimal config
- ✅ **Real-time apps**: Native ReadableStream for SSE
- ✅ **Zero node_modules**: Deno 2.0 caches everything globally

## 🎓 Key Concepts

### 1. Server-Sent Events Format

SSE uses a simple text format over HTTP:

```
data: This is a message\n\n

data: {"json": "works too"}\n\n

data: Multi-line\n
data: messages\n
data: work fine\n\n

: This is a comment (ignored by browser)\n\n
```

Each message ends with `\n\n` (two newlines).

### 2. Automatic Reconnection

If the SSE connection drops, the browser automatically reconnects:

```typescript
metricsEventSource.onerror = () => {
  // Browser automatically attempts to reconnect
  // You just need to handle the error state
  console.log('Connection lost, reconnecting...');
};
```

### 3. Multiple Concurrent Streams

A single client can subscribe to multiple SSE streams:

```typescript
const metricsStream = new EventSource('/api/metrics/stream');
const activityStream = new EventSource('/api/activity/stream');

// Each stream is independent
```

### 4. Broadcasting to All Clients

The server maintains a Set of all connected clients:

```typescript
const clients = new Set<ReadableStreamDefaultController>();

// Add on connect
clients.add(controller);

// Broadcast to all
clients.forEach(controller => {
  controller.enqueue(data);
});

// Remove on disconnect
clients.delete(controller);
```

## 📈 Data Flow Example

**User opens dashboard:**

```
1. Browser:    GET / (initial page load)
               ↓
2. Deno:       Return HTML with window.initialPage
               ↓
3. Switchback: Mount Dashboard component
               ↓
4. Component:  new EventSource('/api/metrics/stream')
               ↓
5. Browser:    GET /api/metrics/stream (SSE connection)
               ↓
6. Deno:       Create ReadableStream, add to metricsClients Set
               ↓
7. Deno:       setInterval every 1s → broadcastMetrics()
               ↓
8. Deno:       Send "data: {...metrics...}\n\n" to all clients
               ↓
9. Browser:    EventSource.onmessage fires
               ↓
10. Component: Update state.metrics, call app.reload()
               ↓
11. Switchback: Re-render with new data (DOM updates)
               ↓
12. Repeat:    New metrics every second (connection stays open!)
```

## 🔄 Comparison with Other Approaches

### SSE (This Recipe)
```typescript
// Server pushes updates
const stream = new EventSource('/api/metrics/stream');
stream.onmessage = (e) => updateUI(e.data);
// ✅ Efficient, automatic, real-time
```

### Polling (Traditional)
```typescript
// Client repeatedly asks
setInterval(async () => {
  const data = await fetch('/api/metrics');
  updateUI(await data.json());
}, 1000);
// ❌ Wasteful, delayed, server load
```

### WebSockets (Overkill for one-way data)
```typescript
// Bidirectional when you only need one-way
const ws = new WebSocket('ws://localhost/metrics');
ws.onmessage = (e) => updateUI(e.data);
// ⚠️  More complex, not always necessary
```

## 🎯 Learning Points

1. **SSE is Perfect for Live Dashboards**
   - Server generates data → pushes to all clients
   - No need for clients to poll repeatedly
   - Automatic reconnection on network issues

2. **Deno Makes SSE Easy**
   - ReadableStream is built-in (no libraries needed)
   - TypeScript support out of the box
   - Clean, modern API

3. **Switchback + SSE = Powerful Combo**
   - Switchback handles SPA navigation (`app.visit()`)
   - SSE handles real-time data updates
   - Together: smooth UX with live updates

4. **Practical Architecture**
   - Frontend: EventSource for SSE, Switchback for routing
   - Backend: ReadableStream for SSE, REST for fallback
   - Result: Real-time dashboard without complex WebSocket setup

## 🚀 Extending This Example

### Add More Metrics

In `server.ts`, extend the `Metrics` type:

```typescript
interface Metrics {
  timestamp: string;
  cpu: number;
  memory: number;
  diskUsage: number;      // NEW
  networkTraffic: number; // NEW
  activeConnections: number;
  totalRequests: number;
  uptime: number;
}
```

### Add Event Filtering

Allow clients to filter events by type:

```typescript
// Client side
const stream = new EventSource('/api/activity/stream?type=error');

// Server side
const type = url.searchParams.get('type');
const shouldBroadcast = !type || activity.type === type;
```

### Add Custom Event Types

SSE supports named events:

```typescript
// Server
controller.enqueue(
  new TextEncoder().encode(`event: metrics\ndata: {...}\n\n`)
);

// Client
stream.addEventListener('metrics', (e) => {
  console.log('Metrics event:', e.data);
});
```

### Add Persistent Storage

Replace in-memory state with a database:

```typescript
import { DB } from "https://deno.land/x/sqlite/mod.ts";

const db = new DB("dashboard.db");
db.query(`
  CREATE TABLE IF NOT EXISTS activities (
    id INTEGER PRIMARY KEY,
    timestamp TEXT,
    type TEXT,
    message TEXT,
    details TEXT
  )
`);
```

### Add Authentication

Protect SSE endpoints with tokens:

```typescript
if (path === '/api/metrics/stream') {
  const token = request.headers.get('Authorization');
  if (!isValidToken(token)) {
    return new Response('Unauthorized', { status: 401 });
  }
  // ... create stream
}
```

## 🛡️ Security Considerations

This is a **demo application** and should not be used in production without additional hardening:

- ⚠️ No authentication on SSE endpoints
- ⚠️ No rate limiting (could DOS with many connections)
- ⚠️ Unbounded client sets (memory leak potential)
- ⚠️ No input validation on POST endpoints
- ⚠️ No HTTPS/TLS support

For production, add:
- **Authentication tokens** for SSE connections
- **Rate limiting** per IP address
- **Maximum connection limits** per client
- **Connection timeouts** for idle clients
- **Input validation** on all POST endpoints
- **HTTPS** with proper certificates
- **CORS policies** for cross-origin requests
- **Logging and monitoring** for abuse detection

## 📚 Learn More About SSE

- [MDN: Server-Sent Events](https://developer.mozilla.org/en-US/docs/Web/API/Server-sent_events)
- [HTML5 SSE Spec](https://html.spec.whatwg.org/multipage/server-sent-events.html)
- [Deno ReadableStream Docs](https://deno.land/api?s=ReadableStream)
- [When to Use SSE vs WebSockets](https://ably.com/topic/websockets-vs-sse)

## 🔍 Comparison with Other Recipes

- **PHP Recipe**: Basic navigation with traditional server-side rendering
- **Zig Recipe**: Modern systems programming with form handling
- **C Recipe**: Optimistic updates for instant UI feedback
- **Go Recipe**: True concurrency with goroutines and worker pools
- **Rust Recipe**: Embedded database with type safety
- **Deno Recipe**: Real-time streaming with Server-Sent Events

Each demonstrates different Switchback integrations. **Deno's SSE support is unique** - perfect for dashboards, notifications, live feeds, and any app that needs server-push updates.

## 🎬 Next Steps

Try modifying the recipe to:
1. Add more metrics (disk I/O, network traffic, etc.)
2. Create different chart types (bar, pie, etc.)
3. Add event filtering by type or search
4. Implement activity persistence with SQLite
5. Add user authentication with JWT tokens
6. Create multiple dashboard views
7. Add export functionality (CSV, JSON)
8. Implement activity alerts/notifications

## 💡 Tips and Tricks

### Debugging SSE Connections

Use browser DevTools Network tab:
1. Filter by "EventStream" type
2. Click on the SSE request
3. View the "Messages" tab to see incoming events
4. Check "EventStream" for connection status

### Testing SSE with curl

```bash
# See raw SSE data
curl -N http://localhost:8000/api/metrics/stream

# -N disables buffering so you see events in real-time
```

### Handling Page Visibility

Pause streams when tab is hidden:

```typescript
document.addEventListener('visibilitychange', () => {
  if (document.hidden) {
    metricsEventSource?.close();
  } else {
    connectMetricsStream(); // Reconnect
  }
});
```

## 🐛 Troubleshooting

**SSE not connecting?**
- Check browser console for errors
- Verify endpoint returns `Content-Type: text/event-stream`
- Use curl to test: `curl -N http://localhost:8000/api/metrics/stream`

**Updates stop after a while?**
- Some proxies/load balancers timeout idle connections
- Add periodic heartbeat comments: `: keepalive\n\n`
- Browser should auto-reconnect, but check `onerror` handler

**Multiple reconnection attempts?**
- Ensure you close old EventSource before creating new one
- Use a single global EventSource per stream
- Check for multiple component mounts

**Memory usage growing?**
- Limit `metricsHistory` size (already done in recipe)
- Limit activity log size (already done in recipe)
- Clean up EventSource on component unmount

**Docker build fails?**
- Ensure you're running from project root: `docker-compose -f examples/recipes/deno/docker-compose.yml up`
- Or cd into the recipe directory first
- Check Deno version in Dockerfile (should be 2.1+)

**Port 8000 already in use?**
- Change port in both server.ts and docker-compose.yml
- Or stop the conflicting process: `lsof -ti:8000 | xargs kill`

---

**Built with ❤️ to showcase Deno + Switchback real-time integration**

Enjoy exploring real-time web applications with Server-Sent Events! 🦕📡⚡
