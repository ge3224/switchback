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

  header {
    background: #161b22;
    border-bottom: 2px solid #f97316;
    padding: 2rem;
    text-align: center;
    box-shadow: 0 4px 12px rgba(249, 115, 22, 0.1);
  }

  header h1 {
    font-size: 2.5rem;
    margin: 0;
  }

  header .subtitle {
    color: #8b949e;
    margin-top: 0.5rem;
    font-size: 1.1rem;
  }

  main {
    max-width: 1200px;
    margin: 2rem auto;
    padding: 0 2rem;
  }


  h1 {
    font-size: 2.5rem;
    color: #f97316;
    text-shadow: 0 0 20px rgba(249, 115, 22, 0.3);
  }

  h2 {
    font-size: 1.8rem;
    color: #f97316;
    margin: 2rem 0 1rem 0;
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

  .intro {
    font-size: 1.1rem;
    margin: 2rem 0;
    line-height: 1.8;
  }
`;
document.head.appendChild(style);

// Simple layout wrapper
function Layout(children) {
  const header = h('header', {},
    h('h1', {},
      '⚡ Zig + Switchback',
      h('span', { class: 'badge' }, 'Form Demo')
    ),
    h('div', { class: 'subtitle' }, 'Server-side form handling without page reloads')
  );

  const main = h('main', {}, children);

  const container = h('div', {});
  container.appendChild(header);
  container.appendChild(main);
  return container;
}

// Page Components
const pages = {
  'Home': (props) => Layout(
    h('div', {},
      h('div', { class: 'intro' },
        'This demo showcases ',
        h('strong', { style: { color: '#f97316' } }, 'form handling with POST requests'),
        ' using a Zig HTTP server. Submit the form below and watch Switchback handle it without a page reload!'
      ),
      h('div', { class: 'stats' },
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.submissions),
          h('div', {}, 'Form Submissions')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.framework),
          h('div', {}, 'Backend')
        )
      ),
      h('h2', {}, '📝 Try It Out'),
      h('form', { method: 'POST', action: '/', 'data-swbk': '', enctype: 'application/x-www-form-urlencoded' },
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

        h('button', { type: 'submit' }, '⚡ Submit via Switchback')
      ),
      props.recentSubmissions && props.recentSubmissions.length > 0 && (
        h('div', { style: { marginTop: '3rem' } },
          h('h2', {}, 'Recent Submissions'),
          h('div', { class: 'user-list' },
            ...props.recentSubmissions.map(sub =>
              h('div', { class: 'user-card' },
                h('h3', {}, sub.name),
                h('p', {}, `📧 ${sub.email}`)
              )
            )
          )
        )
      )
    )
  ),

  'Success': (props) => Layout(
    h('div', {},
      h('div', { class: 'success-message' },
        h('strong', {}, '🎉 Form submitted successfully!'),
        h('p', { style: { marginTop: '1rem' } },
          'Notice how the page updated instantly without a full reload? That\'s Switchback handling the POST request via AJAX.'
        )
      ),
      h('div', { class: 'terminal-box' },
        `Submitted: ${props.submitted.name} (${props.submitted.email})`
      ),
      h('div', { class: 'stats' },
        h('div', { class: 'stat-card' },
          h('strong', {}, props.totalSubmissions),
          h('div', {}, 'Total Submissions')
        )
      ),
      h('div', { style: { marginTop: '2rem' } },
        h('button', {
          class: 'btn',
          onClick: () => window.location.reload()
        }, '📝 Submit Another')
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
    color: '#f97316',  // Orange progress bar
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('⚡ Zig Recipe initialized with form handling support!');
