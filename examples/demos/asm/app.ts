/**
 * Client-side app for ARM64 Assembly demo
 * Environmental Monitoring Station HUD
 */

import { newSwitchback } from '../../../src/index.ts';

// Simple JSX-like helper
function h(tag, props = {}, ...children) {
  const element = document.createElement(tag);

  Object.keys(props || {}).forEach(key => {
    if (key.startsWith('on')) {
      element.addEventListener(key.slice(2).toLowerCase(), props[key]);
    } else if (key === 'class' || key === 'className') {
      element.className = props[key];
    } else if (key === 'style' && typeof props[key] === 'object') {
      Object.assign(element.style, props[key]);
    } else {
      element.setAttribute(key, props[key]);
    }
  });

  children.flat().filter(Boolean).forEach(child => {
    if (typeof child === 'string' || typeof child === 'number') {
      element.appendChild(document.createTextNode(String(child)));
    } else if (child instanceof Node) {
      element.appendChild(child);
    }
  });

  return element;
}

// Format uptime seconds to human readable
function formatUptime(seconds: number): string {
  const days = Math.floor(seconds / 86400);
  const hours = Math.floor((seconds % 86400) / 3600);
  const mins = Math.floor((seconds % 3600) / 60);
  const secs = Math.floor(seconds % 60);

  if (days > 0) return `${days}d ${hours}h ${mins}m`;
  if (hours > 0) return `${hours}h ${mins}m ${secs}s`;
  return `${mins}m ${secs}s`;
}

// Inject terminal HUD theme
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: 'Courier New', 'Consolas', 'Monaco', monospace;
    background: #000;
    color: #0f0;
    line-height: 1.4;
    min-height: 100vh;
    padding: 1rem;
  }

  nav {
    background: #001100;
    border: 2px solid #0f0;
    padding: 0.75rem 1.5rem;
    margin-bottom: 1rem;
    font-size: 0.9rem;
  }

  nav a {
    color: #0f0;
    text-decoration: none;
    margin-right: 2rem;
    padding: 0.25rem 0.5rem;
  }

  nav a:hover {
    background: #003300;
    text-decoration: underline;
  }

  .hud-container {
    max-width: 900px;
    margin: 0 auto;
  }

  .terminal-header {
    border: 2px solid #0f0;
    border-bottom: none;
    padding: 0.5rem 1rem;
    background: #001a00;
    font-weight: bold;
    text-align: center;
  }

  .terminal-box {
    border: 2px solid #0f0;
    padding: 1.5rem;
    background: #000;
    font-size: 0.95rem;
    margin-bottom: 1rem;
  }

  .terminal-box pre {
    font-family: inherit;
    margin: 0;
    line-height: 1.6;
  }

  .status-nominal { color: #0f0; }
  .status-warning { color: #ff0; }
  .status-critical { color: #f00; }

  .data-grid {
    display: grid;
    grid-template-columns: auto 1fr auto;
    gap: 0.5rem 1rem;
    margin: 1rem 0;
  }

  .data-label {
    color: #0a0;
    text-align: right;
  }

  .data-bar {
    background: #001a00;
    border: 1px solid #0f0;
    height: 1.2rem;
    position: relative;
    overflow: hidden;
  }

  .data-bar-fill {
    background: #0f0;
    height: 100%;
    transition: width 0.3s;
  }

  .data-value {
    color: #0f0;
    font-weight: bold;
    text-align: left;
  }

  .system-status {
    margin: 1rem 0;
    padding: 0.75rem;
    border: 1px solid #0f0;
    background: #001100;
  }

  .blink {
    animation: blink 1s step-end infinite;
  }

  @keyframes blink {
    50% { opacity: 0; }
  }

  .info-text {
    color: #0a0;
    font-size: 0.85rem;
    margin: 1rem 0;
    line-height: 1.6;
  }

  .info-text strong {
    color: #0f0;
  }

  a.link {
    color: #0f0;
    text-decoration: underline;
  }

  a.link:hover {
    background: #003300;
  }

  ul {
    list-style: none;
    margin: 1rem 0;
  }

  ul li::before {
    content: '> ';
    color: #0f0;
  }

  ul li {
    color: #0a0;
    margin: 0.25rem 0;
  }

  code {
    background: #001a00;
    padding: 0.2rem 0.4rem;
    border: 1px solid #0a0;
    color: #0f0;
  }
`;
document.head.appendChild(style);

// Layout component
function Layout(children) {
  const nav = h('nav', {},
    h('a', { href: '/', 'data-swbk': '' }, '[DASHBOARD]'),
    h('a', { href: '/about', 'data-swbk': '' }, '[ABOUT]'),
    h('span', { style: { float: 'right', color: '#0a0' } }, 'ENVIRONMENTAL MONITORING STATION')
  );

  const container = h('div', { class: 'hud-container' });
  container.appendChild(nav);
  container.appendChild(children);
  return container;
}

// Page Components
const pages = {
  'Dashboard': (props) => {
    const uptime = props.uptime || 0;
    const load = props.load || 0;
    const deviceId = props.deviceId || 'UNKNOWN';

    // Calculate load percentage (assuming max load of 4.00)
    const loadPercent = Math.min(100, (load / 400) * 100);
    const loadStatus = load < 100 ? 'NOMINAL' : load < 200 ? 'ELEVATED' : 'HIGH';
    const loadClass = load < 100 ? 'status-nominal' : load < 200 ? 'status-warning' : 'status-critical';

    return Layout(
      h('div', {},
        h('div', { class: 'terminal-header' },
          '╔═══════════════════════════════════════════════════════════════╗'
        ),
        h('div', { class: 'terminal-box' },
          h('pre', {},
            '║ ENVIRONMENTAL MONITORING STATION - x86-64 ASSEMBLY            ║\n',
            '║                                                               ║\n',
            `║ DEVICE ID: ${deviceId.padEnd(50)} ║\n`,
            `║ RUNTIME:   ${formatUptime(uptime).padEnd(50)} ║\n`,
            '║                                                               ║\n',
            '╠═══════════════════════════════════════════════════════════════╣\n',
            '║ SYSTEM TELEMETRY                                              ║\n',
            '╠═══════════════════════════════════════════════════════════════╣'
          ),
          h('div', { class: 'data-grid' },
            h('div', { class: 'data-label' }, 'PROC LOAD:'),
            h('div', { class: 'data-bar' },
              h('div', { class: 'data-bar-fill', style: { width: `${loadPercent}%` } })
            ),
            h('div', { class: 'data-value' }, `${(load / 100).toFixed(2)} [${loadStatus}]`),

            h('div', { class: 'data-label' }, 'UPTIME:'),
            h('div', { class: 'data-bar' },
              h('div', { class: 'data-bar-fill', style: { width: '100%' } })
            ),
            h('div', { class: 'data-value' }, `${uptime}s`)
          ),
          h('pre', {},
            '╠═══════════════════════════════════════════════════════════════╣\n',
            '║ STATUS                                                        ║\n',
            '╚═══════════════════════════════════════════════════════════════╝'
          ),
          h('div', { class: 'system-status' },
            h('span', { class: loadClass },
              `● SYSTEM ${loadStatus}`,
              h('span', { class: 'blink' }, ' █')
            )
          )
        ),
        h('div', { class: 'info-text' },
          h('strong', {}, '[ INFO ]'), ' This data is retrieved in real-time from ',
          h('code', {}, '/proc/uptime'), ' and ', h('code', {}, '/proc/loadavg'),
          ' using direct x86-64 assembly syscalls. No frameworks, no libraries - just bare metal code.'
        )
      )
    );
  },

  'About': (props) => Layout(
    h('div', {},
      h('div', { class: 'terminal-header' },
        '╔═══════════════════════════════════════════════════════════════╗'
      ),
      h('div', { class: 'terminal-box' },
        h('pre', {},
          '║ ABOUT THIS SYSTEM                                             ║\n',
          '╠═══════════════════════════════════════════════════════════════╣\n',
          '║                                                               ║\n',
          `║ VERSION:      ${(props.version || 'N/A').padEnd(47)} ║\n`,
          `║ ARCHITECTURE: ${(props.architecture || 'N/A').padEnd(47)} ║\n`,
          '║                                                               ║\n',
          '╠═══════════════════════════════════════════════════════════════╣\n',
          '║ BACKEND INFORMATION                                           ║\n',
          '╚═══════════════════════════════════════════════════════════════╝'
        ),
        h('div', { class: 'info-text' },
          h('strong', {}, props.backend || 'Unknown backend')
        ),
        h('div', { class: 'info-text' },
          h('strong', {}, '[ SYSTEM CALLS USED ]')
        ),
        h('ul', {},
          h('li', {}, h('code', {}, 'openat(56)'), ' - Open /proc files'),
          h('li', {}, h('code', {}, 'read(63)'), ' - Read system statistics'),
          h('li', {}, h('code', {}, 'write(64)'), ' - Send HTTP responses'),
          h('li', {}, h('code', {}, 'socket(198)'), ' - Create TCP socket'),
          h('li', {}, h('code', {}, 'bind(200)'), ' - Bind to port 8000'),
          h('li', {}, h('code', {}, 'listen(201)'), ' - Listen for connections'),
          h('li', {}, h('code', {}, 'accept(202)'), ' - Accept incoming requests'),
          h('li', {}, h('code', {}, 'close(57)'), ' - Close file descriptors')
        ),
        h('div', { class: 'info-text', style: { marginTop: '1.5rem' } },
          h('strong', {}, '[ WHY ASSEMBLY? ]')
        ),
        h('div', { class: 'info-text' },
          'This demonstration proves that Switchback is truly backend-agnostic. ',
          'If it works with raw x86-64 assembly making direct Linux kernel calls, ',
          'it works with ', h('strong', {}, 'any'), ' backend technology.'
        ),
        h('div', { style: { marginTop: '1.5rem' } },
          h('a', { href: '/', 'data-swbk': '', class: 'link' }, '[RETURN TO DASHBOARD]')
        )
      )
    )
  ),

  'Error': (props) => Layout(
    h('div', {},
      h('div', { class: 'terminal-header' },
        '╔═══════════════════════════════════════════════════════════════╗'
      ),
      h('div', { class: 'terminal-box' },
        h('pre', { class: 'status-critical' },
          '║ ERROR                                                         ║\n',
          '╚═══════════════════════════════════════════════════════════════╝\n\n',
          `  ${props.message || 'Unknown error'}`
        ),
        h('div', { style: { marginTop: '1.5rem' } },
          h('a', { href: '/', 'data-swbk': '', class: 'link' }, '[RETURN TO DASHBOARD]')
        )
      )
    )
  ),
};

// Initialize Switchback
const app = newSwitchback({
  resolve: (name) => {
    const component = pages[name];
    if (!component) {
      throw new Error(`Component "${name}" not found`);
    }
    return component;
  },

  setup: ({ el, App, props }) => {
    el.innerHTML = '';
    el.appendChild(App(props));
  },

  initialPage: window.initialPage,

  progress: {
    delay: 250,
    color: '#0f0',
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('[SYSTEM] x86-64 Assembly Environmental Monitoring Station initialized');
console.log('[SYSTEM] All telemetry systems operational');
