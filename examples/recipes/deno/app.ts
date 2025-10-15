import { newSwitchback } from '../../../src/index.ts';

console.log('🦕 Deno Real-Time Dashboard starting...');

// Simple JSX-like helper
function h(tag: string, props: any = {}, ...children: any[]) {
  const element = document.createElement(tag);

  Object.keys(props || {}).forEach(key => {
    if (key.startsWith('on')) {
      element.addEventListener(key.slice(2).toLowerCase(), props[key]);
    } else if (key === 'class' || key === 'className') {
      element.className = props[key];
    } else if (key === 'style' && typeof props[key] === 'object') {
      Object.assign(element.style, props[key]);
    } else if (key !== 'ref') {
      element.setAttribute(key, props[key]);
    }
  });

  // Handle ref callback
  if (props?.ref) {
    props.ref(element);
  }

  children.flat().filter(Boolean).forEach(child => {
    if (typeof child === 'string' || typeof child === 'number') {
      element.appendChild(document.createTextNode(String(child)));
    } else if (child instanceof Node) {
      element.appendChild(child);
    }
  });

  return element;
}

// Global state
const state = {
  metrics: {
    timestamp: '',
    cpu: 0,
    memory: 0,
    activeConnections: 0,
    totalRequests: 0,
    uptime: 0
  },
  activities: [] as any[],
  metricsHistory: [] as any[],
  maxHistorySize: 60, // Keep 60 seconds of history
};

// SSE connections
let metricsEventSource: EventSource | null = null;
let activityEventSource: EventSource | null = null;

// Inject styles
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: #333;
    line-height: 1.6;
    min-height: 100vh;
  }

  nav {
    background: rgba(255, 255, 255, 0.1);
    backdrop-filter: blur(10px);
    border-bottom: 1px solid rgba(255, 255, 255, 0.2);
    color: white;
    padding: 1rem 2rem;
    position: sticky;
    top: 0;
    z-index: 100;
  }

  nav .nav-content {
    max-width: 1400px;
    margin: 0 auto;
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 2rem;
  }

  nav h1 {
    font-size: 1.5rem;
    font-weight: 700;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  nav .nav-links {
    display: flex;
    gap: 1.5rem;
  }

  nav a {
    color: white;
    text-decoration: none;
    font-weight: 500;
    padding: 0.5rem 1rem;
    border-radius: 8px;
    transition: background 0.2s;
  }

  nav a:hover, nav a.active {
    background: rgba(255, 255, 255, 0.2);
  }

  main {
    max-width: 1400px;
    margin: 0 auto;
    padding: 2rem;
  }

  .dashboard-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(300px, 1fr));
    gap: 1.5rem;
    margin-bottom: 2rem;
  }

  .card {
    background: white;
    border-radius: 16px;
    padding: 1.5rem;
    box-shadow: 0 10px 40px rgba(0,0,0,0.1);
    transition: transform 0.2s, box-shadow 0.2s;
  }

  .card:hover {
    transform: translateY(-2px);
    box-shadow: 0 15px 50px rgba(0,0,0,0.15);
  }

  .card-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-bottom: 1rem;
  }

  .card-title {
    font-size: 0.9rem;
    font-weight: 600;
    color: #666;
    text-transform: uppercase;
    letter-spacing: 0.5px;
  }

  .card-icon {
    font-size: 1.5rem;
  }

  .metric-value {
    font-size: 3rem;
    font-weight: 700;
    color: #667eea;
    line-height: 1;
    margin-bottom: 0.5rem;
  }

  .metric-label {
    font-size: 0.9rem;
    color: #999;
  }

  .metric-bar {
    width: 100%;
    height: 8px;
    background: #f0f0f0;
    border-radius: 4px;
    overflow: hidden;
    margin-top: 1rem;
  }

  .metric-bar-fill {
    height: 100%;
    background: linear-gradient(90deg, #667eea 0%, #764ba2 100%);
    transition: width 0.3s ease;
    border-radius: 4px;
  }

  .metric-bar-fill.warning {
    background: linear-gradient(90deg, #f093fb 0%, #f5576c 100%);
  }

  .activity-feed {
    background: white;
    border-radius: 16px;
    padding: 1.5rem;
    box-shadow: 0 10px 40px rgba(0,0,0,0.1);
    max-height: 600px;
    overflow-y: auto;
  }

  .activity-feed h2 {
    font-size: 1.2rem;
    margin-bottom: 1rem;
    color: #333;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .activity-item {
    padding: 1rem;
    border-left: 3px solid #667eea;
    background: #f9f9f9;
    border-radius: 8px;
    margin-bottom: 0.75rem;
    animation: slideIn 0.3s ease;
  }

  @keyframes slideIn {
    from {
      opacity: 0;
      transform: translateX(-20px);
    }
    to {
      opacity: 1;
      transform: translateX(0);
    }
  }

  .activity-item.info {
    border-left-color: #667eea;
  }

  .activity-item.success {
    border-left-color: #51cf66;
  }

  .activity-item.warning {
    border-left-color: #ffd43b;
  }

  .activity-item.error {
    border-left-color: #ff6b6b;
  }

  .activity-header {
    display: flex;
    justify-content: space-between;
    align-items: start;
    margin-bottom: 0.25rem;
  }

  .activity-message {
    font-weight: 600;
    color: #333;
    font-size: 0.95rem;
  }

  .activity-time {
    font-size: 0.8rem;
    color: #999;
    white-space: nowrap;
  }

  .activity-details {
    font-size: 0.85rem;
    color: #666;
    margin-top: 0.25rem;
  }

  .status-badge {
    display: inline-flex;
    align-items: center;
    gap: 0.5rem;
    padding: 0.5rem 1rem;
    border-radius: 20px;
    font-size: 0.85rem;
    font-weight: 600;
    margin-top: 0.5rem;
  }

  .status-badge.connected {
    background: #d3f9d8;
    color: #2b8a3e;
  }

  .status-badge.disconnected {
    background: #ffe3e3;
    color: #c92a2a;
  }

  .pulse {
    display: inline-block;
    width: 8px;
    height: 8px;
    border-radius: 50%;
    background: currentColor;
    animation: pulse 2s infinite;
  }

  @keyframes pulse {
    0%, 100% {
      opacity: 1;
    }
    50% {
      opacity: 0.4;
    }
  }

  .chart-container {
    width: 100%;
    height: 150px;
    margin-top: 1rem;
    position: relative;
  }

  .chart {
    width: 100%;
    height: 100%;
  }

  .hero {
    background: rgba(255, 255, 255, 0.95);
    backdrop-filter: blur(10px);
    border-radius: 16px;
    padding: 3rem 2rem;
    text-align: center;
    margin-bottom: 2rem;
    box-shadow: 0 10px 40px rgba(0,0,0,0.1);
  }

  .hero h2 {
    font-size: 2.5rem;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    -webkit-background-clip: text;
    -webkit-text-fill-color: transparent;
    background-clip: text;
    margin-bottom: 1rem;
  }

  .hero p {
    font-size: 1.1rem;
    color: #666;
    margin-bottom: 0.5rem;
  }

  .tech-badge {
    display: inline-block;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 0.5rem 1rem;
    border-radius: 20px;
    margin: 0.5rem;
    font-size: 0.9rem;
    font-weight: 500;
  }

  .info-box {
    background: rgba(255, 255, 255, 0.95);
    backdrop-filter: blur(10px);
    border-left: 4px solid #667eea;
    padding: 1.5rem;
    border-radius: 12px;
    margin: 2rem 0;
    box-shadow: 0 10px 40px rgba(0,0,0,0.1);
  }

  .info-box h3 {
    color: #667eea;
    margin-bottom: 0.5rem;
  }

  .info-box ul {
    margin-left: 1.5rem;
    color: #555;
  }

  .info-box li {
    margin: 0.5rem 0;
  }

  .add-activity-form {
    background: rgba(255, 255, 255, 0.95);
    backdrop-filter: blur(10px);
    border-radius: 16px;
    padding: 1.5rem;
    margin-bottom: 2rem;
    box-shadow: 0 10px 40px rgba(0,0,0,0.1);
  }

  .add-activity-form h3 {
    margin-bottom: 1rem;
    color: #333;
  }

  .form-group {
    margin-bottom: 1rem;
  }

  .form-group label {
    display: block;
    margin-bottom: 0.5rem;
    font-weight: 600;
    color: #666;
  }

  .form-group input,
  .form-group select,
  .form-group textarea {
    width: 100%;
    padding: 0.75rem;
    border: 2px solid #e0e0e0;
    border-radius: 8px;
    font-size: 1rem;
    font-family: inherit;
    transition: border-color 0.2s;
  }

  .form-group input:focus,
  .form-group select:focus,
  .form-group textarea:focus {
    outline: none;
    border-color: #667eea;
  }

  .btn {
    padding: 0.75rem 1.5rem;
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border: none;
    border-radius: 8px;
    font-size: 1rem;
    font-weight: 600;
    cursor: pointer;
    transition: transform 0.2s, box-shadow 0.2s;
    box-shadow: 0 4px 12px rgba(102, 126, 234, 0.3);
  }

  .btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 6px 20px rgba(102, 126, 234, 0.4);
  }

  .btn:active {
    transform: translateY(0);
  }
`;
document.head.appendChild(style);

// Utility functions
function formatTime(isoString: string): string {
  const date = new Date(isoString);
  const now = new Date();
  const diff = Math.floor((now.getTime() - date.getTime()) / 1000);

  if (diff < 60) return `${diff}s ago`;
  if (diff < 3600) return `${Math.floor(diff / 60)}m ago`;
  if (diff < 86400) return `${Math.floor(diff / 3600)}h ago`;
  return date.toLocaleDateString();
}

function formatUptime(seconds: number): string {
  const hours = Math.floor(seconds / 3600);
  const minutes = Math.floor((seconds % 3600) / 60);
  const secs = seconds % 60;

  if (hours > 0) return `${hours}h ${minutes}m ${secs}s`;
  if (minutes > 0) return `${minutes}m ${secs}s`;
  return `${secs}s`;
}

// SSE management
function connectMetricsStream() {
  if (metricsEventSource) {
    metricsEventSource.close();
  }

  metricsEventSource = new EventSource('/api/metrics/stream');

  metricsEventSource.onmessage = (event) => {
    const metrics = JSON.parse(event.data);
    state.metrics = metrics;

    // Add to history for charting
    state.metricsHistory.push({
      timestamp: Date.now(),
      cpu: metrics.cpu,
      memory: metrics.memory
    });

    // Keep only recent history
    if (state.metricsHistory.length > state.maxHistorySize) {
      state.metricsHistory.shift();
    }

    // Manually update DOM instead of full reload
    updateMetricsUI();
  };

  metricsEventSource.onerror = () => {
    console.error('Metrics stream disconnected');
    metricsEventSource?.close();
    metricsEventSource = null;
  };
}

function connectActivityStream() {
  if (activityEventSource) {
    activityEventSource.close();
  }

  activityEventSource = new EventSource('/api/activity/stream');

  activityEventSource.onmessage = (event) => {
    const activity = JSON.parse(event.data);

    // Check if activity already exists (avoid duplicates)
    const exists = state.activities.some((a: any) => a.id === activity.id);
    if (!exists) {
      // Add to the beginning of the list
      state.activities.unshift(activity);

      // Keep only recent activities (100 max)
      if (state.activities.length > 100) {
        state.activities = state.activities.slice(0, 100);
      }

      // Manually update DOM instead of full reload
      updateActivityUI();
    }
  };

  activityEventSource.onerror = () => {
    console.error('Activity stream disconnected');
    activityEventSource?.close();
    activityEventSource = null;
  };
}

function disconnectStreams() {
  if (metricsEventSource) {
    metricsEventSource.close();
    metricsEventSource = null;
  }
  if (activityEventSource) {
    activityEventSource.close();
    activityEventSource = null;
  }
}

// DOM update functions for SSE (avoid full page reload)
function updateMetricsUI() {
  // Update CPU value
  const cpuValue = document.querySelector('.card:nth-child(1) .metric-value');
  if (cpuValue) cpuValue.textContent = `${Math.round(state.metrics.cpu)}%`;

  // Update CPU bar
  const cpuBar = document.querySelector('.card:nth-child(1) .metric-bar-fill') as HTMLElement;
  if (cpuBar) {
    cpuBar.style.width = `${state.metrics.cpu}%`;
    cpuBar.className = state.metrics.cpu > 80 ? 'metric-bar-fill warning' : 'metric-bar-fill';
  }

  // Update Memory value
  const memValue = document.querySelector('.card:nth-child(2) .metric-value');
  if (memValue) memValue.textContent = `${Math.round(state.metrics.memory)}%`;

  // Update Memory bar
  const memBar = document.querySelector('.card:nth-child(2) .metric-bar-fill') as HTMLElement;
  if (memBar) {
    memBar.style.width = `${state.metrics.memory}%`;
    memBar.className = state.metrics.memory > 80 ? 'metric-bar-fill warning' : 'metric-bar-fill';
  }

  // Update Active Connections
  const connections = document.querySelector('.card:nth-child(3) .metric-value');
  if (connections) connections.textContent = state.metrics.activeConnections.toString();

  // Update Total Requests
  const requests = document.querySelector('.card:nth-child(4) .metric-value');
  if (requests) requests.textContent = state.metrics.totalRequests.toString();

  // Update Uptime
  const uptime = document.querySelector('.card:nth-child(5) .metric-value');
  if (uptime) uptime.textContent = formatUptime(state.metrics.uptime);

  // Update charts if they exist
  const cpuChartContainer = document.querySelector('.card:nth-child(1) .chart-container');
  if (cpuChartContainer && state.metricsHistory.length > 5) {
    cpuChartContainer.innerHTML = '';
    cpuChartContainer.appendChild(createMiniChart(state.metricsHistory, 'cpu'));
  }

  const memChartContainer = document.querySelector('.card:nth-child(2) .chart-container');
  if (memChartContainer && state.metricsHistory.length > 5) {
    memChartContainer.innerHTML = '';
    memChartContainer.appendChild(createMiniChart(state.metricsHistory, 'memory'));
  }
}

function updateActivityUI() {
  // Update dashboard activity feed (recent 10)
  const dashboardFeed = document.querySelector('main .activity-feed');
  if (dashboardFeed && window.location.pathname === '/') {
    const h2 = dashboardFeed.querySelector('h2');
    dashboardFeed.innerHTML = '';
    if (h2) dashboardFeed.appendChild(h2);

    state.activities.slice(0, 10).forEach((activity: any) => {
      dashboardFeed.appendChild(
        h('div', { class: `activity-item ${activity.type}` },
          h('div', { class: 'activity-header' },
            h('div', { class: 'activity-message' }, activity.message),
            h('div', { class: 'activity-time' }, formatTime(activity.timestamp))
          ),
          activity.details ? h('div', { class: 'activity-details' }, activity.details) : null
        )
      );
    });
  }

  // Update activity page feed (all activities)
  const activityPageFeed = document.querySelector('main .activity-feed[style*="maxHeight: none"]');
  if (activityPageFeed && window.location.pathname === '/activity') {
    // Update count in hero
    const heroCount = document.querySelector('.hero p');
    if (heroCount) heroCount.textContent = `${state.activities.length} activities tracked`;

    // Update feed title
    const h2 = activityPageFeed.querySelector('h2');
    if (h2) h2.textContent = `All Activities (${state.activities.length})`;

    // Rebuild activity list
    activityPageFeed.innerHTML = '';
    if (h2) activityPageFeed.appendChild(h2);

    state.activities.forEach((activity: any) => {
      activityPageFeed.appendChild(
        h('div', { class: `activity-item ${activity.type}` },
          h('div', { class: 'activity-header' },
            h('div', { class: 'activity-message' }, activity.message),
            h('div', { class: 'activity-time' }, formatTime(activity.timestamp))
          ),
          activity.details ? h('div', { class: 'activity-details' }, activity.details) : null
        )
      );
    });
  }
}

// API functions
async function addCustomActivity(type: string, message: string, details: string) {
  await fetch('/api/activity', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ type, message, details })
  });
}

// Simple line chart using canvas
function createMiniChart(data: Array<{ cpu: number; memory: number }>, metric: 'cpu' | 'memory') {
  const canvas = document.createElement('canvas');
  canvas.className = 'chart';
  canvas.width = 400;
  canvas.height = 150;

  const ctx = canvas.getContext('2d');
  if (!ctx || data.length === 0) return canvas;

  const values = data.map(d => d[metric]);
  const max = Math.max(...values, 100);
  const min = 0;
  const padding = 10;
  const width = canvas.width - padding * 2;
  const height = canvas.height - padding * 2;

  // Draw grid
  ctx.strokeStyle = '#f0f0f0';
  ctx.lineWidth = 1;
  for (let i = 0; i <= 4; i++) {
    const y = padding + (height / 4) * i;
    ctx.beginPath();
    ctx.moveTo(padding, y);
    ctx.lineTo(canvas.width - padding, y);
    ctx.stroke();
  }

  // Draw line
  ctx.strokeStyle = metric === 'cpu' ? '#667eea' : '#764ba2';
  ctx.lineWidth = 2;
  ctx.beginPath();

  values.forEach((value, i) => {
    const x = padding + (width / (values.length - 1 || 1)) * i;
    const y = canvas.height - padding - ((value - min) / (max - min)) * height;

    if (i === 0) {
      ctx.moveTo(x, y);
    } else {
      ctx.lineTo(x, y);
    }
  });

  ctx.stroke();

  return canvas;
}

// Layout component
function Layout(children: Node, currentPath: string = '/') {
  const nav = h('nav', {},
    h('div', { class: 'nav-content' },
      h('h1', {},
        h('span', {}, '🦕'),
        'Deno Real-Time Dashboard'
      ),
      h('div', { class: 'nav-links' },
        h('a', {
          href: '/',
          class: currentPath === '/' ? 'active' : '',
          onClick: (e: Event) => {
            e.preventDefault();
            app.visit('/');
          }
        }, 'Dashboard'),
        h('a', {
          href: '/activity',
          class: currentPath === '/activity' ? 'active' : '',
          onClick: (e: Event) => {
            e.preventDefault();
            app.visit('/activity');
          }
        }, 'Activity Log')
      )
    )
  );

  const main = h('main', {}, children);

  const container = h('div', {});
  container.appendChild(nav);
  container.appendChild(main);
  return container;
}

// Page Components
const pages: Record<string, (props: any) => Node> = {
  'Dashboard': (props: any) => {
    // Initialize state with props only if empty
    if (props.initialMetrics) {
      state.metrics = props.initialMetrics;
    }
    if (props.initialActivities && state.activities.length === 0) {
      state.activities = props.initialActivities;
    }

    // Connect to streams on mount
    if (!metricsEventSource) {
      connectMetricsStream();
    }
    if (!activityEventSource) {
      connectActivityStream();
    }

    const metricsConnected = metricsEventSource !== null;
    const activityConnected = activityEventSource !== null;

    return Layout(
      h('div', {},
        h('div', { class: 'hero' },
          h('h2', {}, '⚡ Real-Time Dashboard'),
          h('p', {}, 'Server-Sent Events streaming live metrics and activity'),
          h('p', {}, 'Watch the dashboard update in real-time without polling!'),
          h('div', {},
            h('span', { class: 'tech-badge' }, '🦕 Deno Backend'),
            h('span', { class: 'tech-badge' }, '📡 Server-Sent Events'),
            h('span', { class: 'tech-badge' }, '⚡ Switchback SPA')
          )
        ),

        h('div', { class: 'dashboard-grid' },
          // CPU Card
          h('div', { class: 'card' },
            h('div', { class: 'card-header' },
              h('div', { class: 'card-title' }, 'CPU Usage'),
              h('div', { class: 'card-icon' }, '🖥️')
            ),
            h('div', { class: 'metric-value' }, `${Math.round(state.metrics.cpu)}%`),
            h('div', { class: 'metric-label' }, 'Processing Power'),
            h('div', { class: 'metric-bar' },
              h('div', {
                class: state.metrics.cpu > 80 ? 'metric-bar-fill warning' : 'metric-bar-fill',
                style: { width: `${state.metrics.cpu}%` }
              })
            ),
            state.metricsHistory.length > 5 ? h('div', { class: 'chart-container' },
              createMiniChart(state.metricsHistory, 'cpu')
            ) : null
          ),

          // Memory Card
          h('div', { class: 'card' },
            h('div', { class: 'card-header' },
              h('div', { class: 'card-title' }, 'Memory Usage'),
              h('div', { class: 'card-icon' }, '💾')
            ),
            h('div', { class: 'metric-value' }, `${Math.round(state.metrics.memory)}%`),
            h('div', { class: 'metric-label' }, 'RAM Allocated'),
            h('div', { class: 'metric-bar' },
              h('div', {
                class: state.metrics.memory > 80 ? 'metric-bar-fill warning' : 'metric-bar-fill',
                style: { width: `${state.metrics.memory}%` }
              })
            ),
            state.metricsHistory.length > 5 ? h('div', { class: 'chart-container' },
              createMiniChart(state.metricsHistory, 'memory')
            ) : null
          ),

          // Connections Card
          h('div', { class: 'card' },
            h('div', { class: 'card-header' },
              h('div', { class: 'card-title' }, 'Active Connections'),
              h('div', { class: 'card-icon' }, '🔗')
            ),
            h('div', { class: 'metric-value' }, state.metrics.activeConnections),
            h('div', { class: 'metric-label' }, 'Current Clients')
          ),

          // Requests Card
          h('div', { class: 'card' },
            h('div', { class: 'card-header' },
              h('div', { class: 'card-title' }, 'Total Requests'),
              h('div', { class: 'card-icon' }, '📊')
            ),
            h('div', { class: 'metric-value' }, state.metrics.totalRequests),
            h('div', { class: 'metric-label' }, 'Since Startup')
          ),

          // Uptime Card
          h('div', { class: 'card' },
            h('div', { class: 'card-header' },
              h('div', { class: 'card-title' }, 'Server Uptime'),
              h('div', { class: 'card-icon' }, '⏱️')
            ),
            h('div', { class: 'metric-value', style: { fontSize: '2rem' } }, formatUptime(state.metrics.uptime)),
            h('div', { class: 'metric-label' }, 'Running Smoothly')
          ),

          // Connection Status Card
          h('div', { class: 'card' },
            h('div', { class: 'card-header' },
              h('div', { class: 'card-title' }, 'Stream Status'),
              h('div', { class: 'card-icon' }, '📡')
            ),
            h('div', {},
              h('div', { class: metricsConnected ? 'status-badge connected' : 'status-badge disconnected' },
                h('span', { class: 'pulse' }),
                metricsConnected ? 'Metrics: Connected' : 'Metrics: Disconnected'
              ),
              h('div', { class: activityConnected ? 'status-badge connected' : 'status-badge disconnected' },
                h('span', { class: 'pulse' }),
                activityConnected ? 'Activity: Connected' : 'Activity: Disconnected'
              )
            )
          )
        ),

        h('div', { class: 'activity-feed' },
          h('h2', {}, '📋 ', 'Recent Activity'),
          ...state.activities.slice(0, 10).map((activity: any) =>
            h('div', { class: `activity-item ${activity.type}` },
              h('div', { class: 'activity-header' },
                h('div', { class: 'activity-message' }, activity.message),
                h('div', { class: 'activity-time' }, formatTime(activity.timestamp))
              ),
              activity.details ? h('div', { class: 'activity-details' }, activity.details) : null
            )
          )
        ),

        h('div', { class: 'info-box' },
          h('h3', {}, '📡 How SSE Works with Switchback'),
          h('p', {}, 'This dashboard demonstrates real-time push updates:'),
          h('ul', {},
            h('li', {}, '📡 Server-Sent Events: Server pushes updates to browser automatically'),
            h('li', {}, '⚡ No Polling: Unlike REST, browser doesn\'t need to ask for updates'),
            h('li', {}, '🔄 Live Data: Metrics update every second, activities appear instantly'),
            h('li', {}, '🦕 Deno Native: Uses Deno\'s built-in ReadableStream API'),
            h('li', {}, '🎯 SPA Navigation: Switchback handles routing, SSE handles real-time data')
          ),
          h('p', { style: { marginTop: '1rem', fontWeight: '500' } }, 'Watch the CPU/Memory charts and activity feed update in real-time!')
        )
      ),
      '/'
    );
  },

  'ActivityPage': (props: any) => {
    // Initialize activities from props
    if (props.activities && state.activities.length === 0) {
      state.activities = props.activities;
    }

    // Connect to activity stream on mount
    if (!activityEventSource) {
      connectActivityStream();
    }

    const activityConnected = activityEventSource !== null;

    return Layout(
      h('div', {},
        h('div', { class: 'hero' },
          h('h2', {}, '📋 Activity Log'),
          h('p', {}, `${state.activities.length} activities tracked`),
          h('div', { class: activityConnected ? 'status-badge connected' : 'status-badge disconnected' },
            h('span', { class: 'pulse' }),
            activityConnected ? 'Live Stream Active' : 'Stream Disconnected'
          )
        ),

        h('div', { class: 'add-activity-form' },
          h('h3', {}, '➕ Add Custom Activity'),
          h('div', { class: 'form-group' },
            h('label', {}, 'Type'),
            h('select', { id: 'activity-type' },
              h('option', { value: 'info' }, 'Info'),
              h('option', { value: 'success' }, 'Success'),
              h('option', { value: 'warning' }, 'Warning'),
              h('option', { value: 'error' }, 'Error')
            )
          ),
          h('div', { class: 'form-group' },
            h('label', {}, 'Message'),
            h('input', { type: 'text', id: 'activity-message', placeholder: 'Activity message...' })
          ),
          h('div', { class: 'form-group' },
            h('label', {}, 'Details (optional)'),
            h('textarea', { id: 'activity-details', rows: '3', placeholder: 'Additional details...' })
          ),
          h('button', {
            class: 'btn',
            onClick: async () => {
              const type = (document.getElementById('activity-type') as HTMLSelectElement).value;
              const message = (document.getElementById('activity-message') as HTMLInputElement).value;
              const details = (document.getElementById('activity-details') as HTMLTextAreaElement).value;

              if (message) {
                await addCustomActivity(type, message, details);

                // Clear form
                (document.getElementById('activity-message') as HTMLInputElement).value = '';
                (document.getElementById('activity-details') as HTMLTextAreaElement).value = '';
              }
            }
          }, 'Add Activity')
        ),

        h('div', { class: 'activity-feed', style: { maxHeight: 'none' } },
          h('h2', {}, `All Activities (${state.activities.length})`),
          ...state.activities.map((activity: any) =>
            h('div', { class: `activity-item ${activity.type}` },
              h('div', { class: 'activity-header' },
                h('div', { class: 'activity-message' }, activity.message),
                h('div', { class: 'activity-time' }, formatTime(activity.timestamp))
              ),
              activity.details ? h('div', { class: 'activity-details' }, activity.details) : null
            )
          )
        )
      ),
      '/activity'
    );
  },

  'NotFound': (props: any) => {
    return Layout(
      h('div', { class: 'hero' },
        h('h2', {}, '404 Not Found'),
        h('p', {}, `The page "${props.path}" does not exist.`),
        h('a', {
          href: '/',
          onClick: (e: Event) => {
            e.preventDefault();
            app.visit('/');
          }
        }, 'Go to Dashboard')
      ),
      props.path
    );
  }
};

// Initialize Switchback
let app: any;

app = newSwitchback({
  resolve: (name: string) => {
    const component = pages[name];
    if (!component) {
      throw new Error(`Component "${name}" not found`);
    }
    return component;
  },

  setup: ({ el, App, props }) => {
    // Clean up old streams when navigating away from dashboard
    const prevPath = window.location.pathname;
    if (prevPath === '/' && props.component !== 'Dashboard') {
      disconnectStreams();
    }

    el.innerHTML = '';
    el.appendChild(App(props));
  },

  initialPage: (window as any).initialPage,

  progress: {
    delay: 250,
    color: '#667eea',
    includeCSS: true,
    showSpinner: true,
  },
});

// Cleanup on page unload
window.addEventListener('beforeunload', () => {
  disconnectStreams();
});

console.log('🦕 Deno Real-Time Dashboard initialized!');
