/**
 * Client-side app for Zig recipe
 * Bundles Switchback directly from source
 * Demonstrates FORM HANDLING with POST requests
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

  children.flat().forEach(child => {
    if (typeof child === 'string' || typeof child === 'number') {
      element.appendChild(document.createTextNode(String(child)));
    } else if (child instanceof Node) {
      element.appendChild(child);
    }
  });

  return element;
}

// Inject dark terminal theme CSS
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: 'Courier New', monospace;
    background: #0d1117;
    color: #c9d1d9;
    line-height: 1.6;
  }

  nav {
    background: #161b22;
    border-bottom: 2px solid #f97316;
    padding: 1rem 2rem;
    position: sticky;
    top: 0;
    z-index: 100;
    box-shadow: 0 4px 12px rgba(249, 115, 22, 0.1);
  }

  nav .nav-content {
    max-width: 1200px;
    margin: 0 auto;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  nav a {
    color: #58a6ff;
    text-decoration: none;
    margin-right: 1.5rem;
    padding: 0.5rem 1rem;
    border: 1px solid transparent;
    transition: all 0.2s;
    font-weight: bold;
  }

  nav a:hover {
    border-color: #f97316;
    background: rgba(249, 115, 22, 0.1);
  }

  .nav-badge {
    background: #f97316;
    color: #0d1117;
    padding: 0.4rem 0.8rem;
    border-radius: 4px;
    font-size: 0.85rem;
    font-weight: bold;
  }

  main {
    max-width: 1200px;
    margin: 2rem auto;
    padding: 0 2rem;
  }

  .demo-hint {
    background: linear-gradient(135deg, #30363d 0%, #21262d 100%);
    border: 2px solid #f97316;
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 2rem;
    text-align: center;
  }

  .demo-hint strong {
    color: #f97316;
    font-size: 1.1rem;
  }

  .hint-flash {
    display: inline-block;
    color: #fbbf24;
    animation: pulse 2s infinite;
  }

  @keyframes pulse {
    0%, 100% { opacity: 1; }
    50% { opacity: 0.5; }
  }

  h1 {
    font-size: 2.5rem;
    margin-bottom: 1rem;
    color: #f97316;
    text-shadow: 0 0 20px rgba(249, 115, 22, 0.3);
  }

  .badge {
    background: linear-gradient(135deg, #f97316, #fbbf24);
    color: #0d1117;
    padding: 0.3rem 0.8rem;
    border-radius: 6px;
    font-size: 1rem;
    margin-left: 1rem;
    display: inline-block;
  }

  .terminal-box {
    background: #0d1117;
    border: 2px solid #30363d;
    border-radius: 8px;
    padding: 1.5rem;
    margin: 1.5rem 0;
    font-family: 'Courier New', monospace;
  }

  .terminal-box::before {
    content: '$ ';
    color: #f97316;
    font-weight: bold;
  }

  form {
    background: #161b22;
    border: 2px solid #30363d;
    border-radius: 8px;
    padding: 2rem;
    margin: 2rem 0;
  }

  form:focus-within {
    border-color: #f97316;
    box-shadow: 0 0 20px rgba(249, 115, 22, 0.2);
  }

  label {
    display: block;
    color: #f97316;
    margin-bottom: 0.5rem;
    font-weight: bold;
  }

  input, textarea {
    width: 100%;
    background: #0d1117;
    border: 1px solid #30363d;
    color: #c9d1d9;
    padding: 0.75rem;
    border-radius: 4px;
    font-family: 'Courier New', monospace;
    margin-bottom: 1.5rem;
  }

  input:focus, textarea:focus {
    outline: none;
    border-color: #f97316;
    box-shadow: 0 0 10px rgba(249, 115, 22, 0.2);
  }

  button, .btn {
    background: linear-gradient(135deg, #f97316, #fbbf24);
    color: #0d1117;
    border: none;
    padding: 0.75rem 1.5rem;
    border-radius: 6px;
    cursor: pointer;
    font-weight: bold;
    font-family: 'Courier New', monospace;
    text-decoration: none;
    display: inline-block;
    transition: all 0.2s;
  }

  button:hover, .btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 12px rgba(249, 115, 22, 0.4);
  }

  .success-message {
    background: linear-gradient(135deg, #10b981, #059669);
    color: white;
    padding: 1.5rem;
    border-radius: 8px;
    margin: 1.5rem 0;
    border-left: 4px solid #34d399;
  }

  .user-list {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
    gap: 1.5rem;
    margin: 2rem 0;
  }

  .user-card {
    background: #161b22;
    border: 2px solid #30363d;
    border-radius: 8px;
    padding: 1.5rem;
    transition: all 0.2s;
  }

  .user-card:hover {
    border-color: #f97316;
    transform: translateY(-4px);
    box-shadow: 0 8px 20px rgba(249, 115, 22, 0.2);
  }

  .user-card h3 {
    color: #f97316;
    margin-bottom: 0.5rem;
  }

  .user-card a {
    color: #58a6ff;
    text-decoration: none;
  }

  .user-card a:hover {
    text-decoration: underline;
  }

  .stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
    margin: 2rem 0;
  }

  .stat-card {
    background: linear-gradient(135deg, #161b22, #0d1117);
    border: 2px solid #30363d;
    border-radius: 8px;
    padding: 1.5rem;
    text-align: center;
  }

  .stat-card strong {
    font-size: 2.5rem;
    color: #f97316;
    display: block;
    margin-bottom: 0.5rem;
  }

  ul {
    margin-left: 2rem;
    margin-top: 1rem;
  }

  li {
    margin: 0.5rem 0;
    color: #8b949e;
  }

  a.back-link {
    color: #58a6ff;
    text-decoration: none;
    display: inline-block;
    margin-top: 2rem;
  }

  a.back-link:hover {
    text-decoration: underline;
  }
`;
document.head.appendChild(style);

// Layout component - PERSISTENT across page changes
function Layout(children) {
  const nav = h('nav', {},
    h('div', { class: 'nav-content' },
      h('div', {},
        h('a', { href: '/', 'data-swbk': '' }, '⚡ Home'),
        h('a', { href: '/users', 'data-swbk': '' }, '👥 Users'),
        h('a', { href: '/submit', 'data-swbk': '' }, '📝 Submit'),
        h('a', { href: '/about', 'data-swbk': '' }, 'ℹ️  About'),
      ),
      h('div', { class: 'nav-badge' }, '🔒 PERSISTENT')
    )
  );

  const demoHint = h('div', { class: 'demo-hint' },
    h('strong', {}, '⚡ Zig + Switchback Demo:'),
    ' This recipe demonstrates FORM HANDLING with POST requests. ',
    h('span', { class: 'hint-flash' }, '✨ Try the Submit page!')
  );

  const main = h('main', {},
    demoHint,
    children
  );

  const container = h('div', {});
  container.appendChild(nav);
  container.appendChild(main);
  return container;
}

// Page Components
const pages = {
  'Home': (props) => Layout(
    h('div', {},
      h('h1', {},
        'Zig Recipe',
        h('span', { class: 'badge' }, '⚡ Zig')
      ),
      h('div', { class: 'terminal-box' },
        props.message
      ),
      h('div', { class: 'stats' },
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.users),
          h('div', {}, 'Total Users')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.submissions),
          h('div', {}, 'Form Submissions')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.framework),
          h('div', {}, 'Backend')
        )
      ),
      h('p', { style: { marginTop: '2rem', fontSize: '1.1rem' } },
        'This recipe showcases Switchback\'s form handling capabilities with a Zig backend.'
      ),
      h('div', { style: { marginTop: '2rem', display: 'flex', gap: '1rem' } },
        h('a', { href: '/submit', 'data-swbk': '', class: 'btn' }, '📝 Try Form Submit'),
        h('a', { href: '/users', 'data-swbk': '', class: 'btn' }, '👥 View Users')
      )
    )
  ),

  'Users/Index': (props) => Layout(
    h('div', {},
      h('h1', {}, '👥 Users'),
      h('div', { class: 'user-list' },
        ...props.users.map(user =>
          h('div', { class: 'user-card' },
            h('h3', {},
              h('a', { href: `/users/${user.id}`, 'data-swbk': '' }, user.name)
            ),
            h('p', {}, `📧 ${user.email}`),
            h('p', { style: { color: '#8b949e', fontSize: '0.9rem', marginTop: '0.5rem' } },
              `Joined: ${user.joined}`
            )
          )
        )
      ),
      h('a', { href: '/', 'data-swbk': '', class: 'back-link' }, '← Back to Home')
    )
  ),

  'Users/Show': (props) => Layout(
    h('div', {},
      h('h1', {}, props.user.name),
      h('div', { class: 'terminal-box' },
        `User #${props.user.id}`
      ),
      h('p', { style: { fontSize: '1.1rem', margin: '1rem 0' } },
        h('strong', { style: { color: '#f97316' } }, '📧 Email: '),
        props.user.email
      ),
      h('p', { style: { fontSize: '1.1rem', margin: '1rem 0' } },
        h('strong', { style: { color: '#f97316' } }, '📅 Joined: '),
        props.user.joined
      ),
      h('a', { href: '/users', 'data-swbk': '', class: 'back-link' }, '← Back to Users')
    )
  ),

  'Submit': (props) => Layout(
    h('div', {},
      h('h1', {}, '📝 Submit Form'),
      h('div', { class: 'terminal-box' },
        'Try submitting this form - Switchback handles it without page reload!'
      ),
      h('form', { method: 'POST', action: '/submit', 'data-swbk': '', enctype: 'application/x-www-form-urlencoded' },
        h('label', { for: 'name' }, '> Name:'),
        h('input', {
          type: 'text',
          id: 'name',
          name: 'name',
          placeholder: 'Enter your name...',
          required: true
        }),

        h('label', { for: 'email' }, '> Email:'),
        h('input', {
          type: 'email',
          id: 'email',
          name: 'email',
          placeholder: 'your@email.com',
          required: true
        }),

        h('label', { for: 'message' }, '> Message:'),
        h('textarea', {
          id: 'message',
          name: 'message',
          rows: 4,
          placeholder: 'Type your message here...',
          required: true
        }),

        h('button', { type: 'submit' }, '⚡ Submit via Switchback')
      ),
      props.recentSubmissions && props.recentSubmissions.length > 0 && (
        h('div', { style: { marginTop: '2rem' } },
          h('h2', { style: { color: '#f97316', marginBottom: '1rem' } }, 'Recent Submissions:'),
          h('div', { class: 'user-list' },
            ...props.recentSubmissions.map(sub =>
              h('div', { class: 'user-card' },
                h('h3', {}, sub.name),
                h('p', {}, `📧 ${sub.email}`),
                h('p', { style: { marginTop: '0.5rem', fontSize: '0.9rem', color: '#8b949e' } },
                  sub.message
                )
              )
            )
          )
        )
      ),
      h('a', { href: '/', 'data-swbk': '', class: 'back-link' }, '← Back to Home')
    )
  ),

  'Submit/Success': (props) => Layout(
    h('div', {},
      h('h1', {}, '✅ Success!'),
      h('div', { class: 'success-message' },
        h('strong', {}, '🎉 Form submitted successfully via Switchback POST!'),
        h('p', { style: { marginTop: '1rem' } },
          'Notice how the page updated instantly without a full reload? That\'s Switchback in action.'
        )
      ),
      h('div', { class: 'terminal-box' },
        `Submitted data: name="${props.submitted.name}", email="${props.submitted.email}"`
      ),
      h('p', { style: { margin: '1.5rem 0', fontSize: '1.1rem' } },
        props.message
      ),
      h('div', { style: { display: 'flex', gap: '1rem' } },
        h('a', { href: '/submit', 'data-swbk': '', class: 'btn' }, '📝 Submit Another'),
        h('a', { href: '/', 'data-swbk': '', class: 'btn' }, '🏠 Home')
      )
    )
  ),

  'About': (props) => Layout(
    h('div', {},
      h('h1', {}, 'ℹ️  About This Recipe'),
      h('div', { class: 'terminal-box' },
        `Zig Recipe v${props.version}`
      ),
      h('p', { style: { margin: '1.5rem 0', fontSize: '1.1rem' } },
        h('strong', { style: { color: '#f97316' } }, '🔧 Backend: '),
        props.backend
      ),
      h('h2', { style: { color: '#f97316', marginTop: '2rem', marginBottom: '1rem' } },
        'Key Features:'
      ),
      h('ul', {},
        ...props.features.map(feature => h('li', {}, `⚡ ${feature}`))
      ),
      h('h2', { style: { color: '#f97316', marginTop: '2rem', marginBottom: '1rem' } },
        'What Makes This Special:'
      ),
      h('div', { class: 'terminal-box' },
        'This recipe demonstrates Switchback\'s FORM HANDLING with POST requests. ',
        'Unlike the PHP recipe (which shows basic navigation), this showcases how Switchback ',
        'intercepts form submissions and handles them via AJAX without page reloads.'
      ),
      h('a', { href: '/', 'data-swbk': '', class: 'back-link' }, '← Back to Home')
    )
  ),

  'Error': (props) => Layout(
    h('div', {},
      h('h1', {}, '❌ Error'),
      h('div', { class: 'terminal-box' },
        `Error: ${props.message}`
      ),
      h('a', { href: '/', 'data-swbk': '', class: 'back-link' }, '← Back to Home')
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
    color: '#f97316',  // Orange progress bar
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('⚡ Zig Recipe initialized with form handling support!');
