#!/usr/bin/env -S deno run --allow-net --allow-read

/**
 * Deno + Switchback Recipe - Real-Time Dashboard with Server-Sent Events
 *
 * This server demonstrates:
 * - Server-Sent Events (SSE) for real-time push updates
 * - Live metrics streaming (CPU, memory, active connections)
 * - Activity log with live updates
 * - Deno's built-in TypeScript and modern APIs
 */

const PORT = 8000;

// Types
interface Activity {
  id: number;
  timestamp: string;
  type: 'info' | 'success' | 'warning' | 'error';
  message: string;
  details?: string;
}

interface Metrics {
  timestamp: string;
  cpu: number;
  memory: number;
  activeConnections: number;
  totalRequests: number;
  uptime: number;
}

// Global state
let activityLog: Activity[] = [];
let activityIdCounter = 1;
let metricsClients: Set<ReadableStreamDefaultController> = new Set();
let activityClients: Set<ReadableStreamDefaultController> = new Set();
let totalRequests = 0;
let activeConnections = 0;
const startTime = Date.now();

// Initialize with some sample activities
activityLog = [
  {
    id: activityIdCounter++,
    timestamp: new Date().toISOString(),
    type: 'success',
    message: 'Server started',
    details: 'Deno server initialized on port 8000'
  },
  {
    id: activityIdCounter++,
    timestamp: new Date().toISOString(),
    type: 'info',
    message: 'Switchback integration ready',
    details: 'SPA routing and SSE endpoints configured'
  }
];

// Utility functions
function addActivity(type: Activity['type'], message: string, details?: string) {
  const activity: Activity = {
    id: activityIdCounter++,
    timestamp: new Date().toISOString(),
    type,
    message,
    details
  };

  activityLog.unshift(activity);
  if (activityLog.length > 100) {
    activityLog = activityLog.slice(0, 100);
  }

  // Broadcast to all activity SSE clients
  broadcastActivity(activity);
}

function broadcastActivity(activity: Activity) {
  const data = `data: ${JSON.stringify(activity)}\n\n`;

  activityClients.forEach(controller => {
    try {
      controller.enqueue(new TextEncoder().encode(data));
    } catch (e) {
      // Client disconnected, will be cleaned up
      activityClients.delete(controller);
    }
  });
}

function broadcastMetrics(metrics: Metrics) {
  const data = `data: ${JSON.stringify(metrics)}\n\n`;

  metricsClients.forEach(controller => {
    try {
      controller.enqueue(new TextEncoder().encode(data));
    } catch (e) {
      // Client disconnected, will be cleaned up
      metricsClients.delete(controller);
    }
  });
}

// Track CPU usage
let lastCpuUsage = 0;
let lastCpuTime = Date.now();

function generateMetrics(): Metrics {
  const now = Date.now();
  const uptime = Math.floor((now - startTime) / 1000);

  // Get real memory usage from Deno
  const memUsage = Deno.memoryUsage();
  const rss = memUsage.rss; // Resident Set Size (physical memory)
  const heapUsed = memUsage.heapUsed;
  const heapTotal = memUsage.heapTotal;

  // Calculate memory percentage (based on heap usage)
  const memoryPercent = (heapUsed / heapTotal) * 100;

  // Estimate CPU usage (Deno doesn't have direct CPU metrics, so we approximate)
  // This is a rough estimate based on event loop lag
  const currentTime = Date.now();
  const timeDelta = currentTime - lastCpuTime;

  // Simple heuristic: more clients and requests = higher CPU
  const activityFactor = (metricsClients.size + activityClients.size) * 5;
  const requestFactor = Math.min((totalRequests / (uptime || 1)) * 2, 30);

  // Smooth CPU usage changes
  const targetCpu = Math.min(activityFactor + requestFactor + 5, 95);
  lastCpuUsage = lastCpuUsage * 0.7 + targetCpu * 0.3; // Exponential smoothing
  lastCpuTime = currentTime;

  return {
    timestamp: new Date().toISOString(),
    cpu: Math.round(lastCpuUsage * 10) / 10, // Round to 1 decimal
    memory: Math.round(memoryPercent * 10) / 10, // Round to 1 decimal
    activeConnections,
    totalRequests,
    uptime
  };
}

// Start metrics broadcast loop
setInterval(() => {
  if (metricsClients.size > 0) {
    const metrics = generateMetrics();
    broadcastMetrics(metrics);
  }
}, 1000); // Send metrics every second

// Simulate random activity
setInterval(() => {
  if (Math.random() < 0.3) { // 30% chance every 3 seconds
    const activities = [
      { type: 'info' as const, message: 'User session started', details: `Session ID: ${Math.random().toString(36).substr(2, 9)}` },
      { type: 'success' as const, message: 'API request completed', details: `Response time: ${Math.floor(Math.random() * 200 + 50)}ms` },
      { type: 'info' as const, message: 'Cache refreshed', details: 'Memory cache updated with latest data' },
      { type: 'warning' as const, message: 'High memory usage detected', details: `Memory usage: ${Math.floor(Math.random() * 20 + 70)}%` },
      { type: 'success' as const, message: 'Database query optimized', details: 'Query execution time reduced by 40%' },
      { type: 'info' as const, message: 'New SSE client connected', details: 'Real-time stream established' },
    ];

    const activity = activities[Math.floor(Math.random() * activities.length)];
    addActivity(activity.type, activity.message, activity.details);
  }
}, 3000);

// Helper to create SSE response
function createSSEStream(clientSet: Set<ReadableStreamDefaultController>) {
  let controller: ReadableStreamDefaultController;

  const stream = new ReadableStream({
    start(ctrl) {
      controller = ctrl;
      clientSet.add(controller);
      activeConnections++; // Count SSE as active connection

      // Send initial heartbeat
      controller.enqueue(new TextEncoder().encode(': heartbeat\n\n'));
    },
    cancel() {
      clientSet.delete(controller);
      activeConnections--; // Decrement when SSE closes
    }
  });

  return stream;
}

// Route handlers
function handlePageRoute(url: URL): { component: string; props: any; url: string } {
  const path = url.pathname;

  if (path === '/' || path === '/dashboard') {
    return {
      component: 'Dashboard',
      props: {
        initialActivities: activityLog.slice(0, 10),
        initialMetrics: generateMetrics()
      },
      url: '/'
    };
  } else if (path === '/activity') {
    return {
      component: 'ActivityPage',
      props: {
        activities: activityLog
      },
      url: '/activity'
    };
  } else {
    return {
      component: 'NotFound',
      props: { path },
      url: path
    };
  }
}

// HTTP request handler
async function handleRequest(request: Request): Promise<Response> {
  totalRequests++;

  const url = new URL(request.url);
  const path = url.pathname;
  const isSwitchback = request.headers.get('X-Switchback');

  console.log(`${request.method} ${path} ${isSwitchback ? '(Switchback)' : ''}`);

  try {
    // SSE endpoint for metrics (connection tracked in createSSEStream)
    if (path === '/api/metrics/stream') {
      addActivity('info', 'Metrics stream connected', 'Client subscribed to real-time metrics');

      const stream = createSSEStream(metricsClients);

      return new Response(stream, {
        headers: {
          'Content-Type': 'text/event-stream',
          'Cache-Control': 'no-cache',
          'Connection': 'keep-alive',
          'Access-Control-Allow-Origin': '*',
        }
      });
    }

    // SSE endpoint for activity log (connection tracked in createSSEStream)
    if (path === '/api/activity/stream') {
      addActivity('success', 'Activity stream connected', 'Client subscribed to live activity updates');

      const stream = createSSEStream(activityClients);

      return new Response(stream, {
        headers: {
          'Content-Type': 'text/event-stream',
          'Cache-Control': 'no-cache',
          'Connection': 'keep-alive',
          'Access-Control-Allow-Origin': '*',
        }
      });
    }

    // REST API endpoints
    if (path === '/api/metrics') {
      return new Response(JSON.stringify(generateMetrics()), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    if (path === '/api/activity') {
      return new Response(JSON.stringify(activityLog), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // POST endpoint to add custom activity
    if (path === '/api/activity' && request.method === 'POST') {
      const body = await request.json();
      addActivity(body.type || 'info', body.message, body.details);

      return new Response(JSON.stringify({ success: true }), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Serve bundled app.js
    if (path === '/dist/app.js') {
      try {
        const file = await Deno.readFile('./dist/app.js');
        return new Response(file, {
          headers: { 'Content-Type': 'application/javascript' }
        });
      } catch {
        return new Response('Build not found. Run: deno task build', { status: 404 });
      }
    }

    // Serve other static files
    if (path.startsWith('/dist/')) {
      try {
        const filePath = `.${path}`;
        const file = await Deno.readFile(filePath);

        const contentType = path.endsWith('.js') ? 'application/javascript' :
                           path.endsWith('.css') ? 'text/css' :
                           'application/octet-stream';

        return new Response(file, {
          headers: { 'Content-Type': contentType }
        });
      } catch {
        return new Response('Not Found', { status: 404 });
      }
    }

    // Handle page routes
    const pageData = handlePageRoute(url);

    // Return JSON for Switchback requests
    if (isSwitchback) {
      return new Response(JSON.stringify(pageData), {
        headers: { 'Content-Type': 'application/json' }
      });
    }

    // Return full HTML for initial page load
    const html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Deno Real-Time Dashboard - Switchback</title>
  <style>
    body {
      margin: 0;
      padding: 0;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
    }
  </style>
</head>
<body>
  <div id="app" data-swbk-app></div>
  <script>
    window.initialPage = ${JSON.stringify(pageData)};
  </script>
  <script type="module" src="/dist/app.js"></script>
</body>
</html>`;

    return new Response(html, {
      headers: { 'Content-Type': 'text/html' }
    });

  } finally {
    // Don't decrement here - SSE connections are tracked in createSSEStream
    // Regular HTTP requests finish immediately so no tracking needed
  }
}

// Start server
console.log(`🦕 Deno server starting on http://localhost:${PORT}`);
console.log('📊 Real-time dashboard with Server-Sent Events');
console.log('⚡ Switchback integration enabled');
console.log('');

const server = Deno.serve({
  port: PORT,
  onListen: () => {
    console.log(`✅ Server running on http://localhost:${PORT}`);
    console.log('');
    console.log('Available endpoints:');
    console.log('  GET  /                     - Dashboard page');
    console.log('  GET  /activity             - Activity log page');
    console.log('  GET  /api/metrics          - Current metrics (REST)');
    console.log('  GET  /api/metrics/stream   - Real-time metrics (SSE)');
    console.log('  GET  /api/activity         - Activity log (REST)');
    console.log('  GET  /api/activity/stream  - Real-time activity (SSE)');
    console.log('  POST /api/activity         - Add custom activity');
    console.log('');
  }
}, handleRequest);

// Graceful shutdown
Deno.addSignalListener("SIGINT", () => {
  console.log('\n🛑 Shutting down gracefully...');

  // Close all SSE connections
  metricsClients.forEach(controller => {
    try {
      controller.close();
    } catch {
      // Ignore errors
    }
  });

  activityClients.forEach(controller => {
    try {
      controller.close();
    } catch {
      // Ignore errors
    }
  });

  metricsClients.clear();
  activityClients.clear();

  console.log('✅ All connections closed');
  Deno.exit(0);
});
