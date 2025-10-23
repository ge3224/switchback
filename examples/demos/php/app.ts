/**
 * Client-side app for PHP recipe
 * Bundles Switchback directly from source
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

// Layout component
function Layout(children) {
  const nav = h('nav', { title: 'This navigation persists across page changes - it never reloads' },
    h('div', { class: 'nav-content' },
      h('div', {},
        h('a', { href: '/', 'data-swbk': '' }, 'Home'),
        h('a', { href: '/users', 'data-swbk': '' }, 'Users'),
        h('a', { href: '/about', 'data-swbk': '' }, 'About'),
      ),
      h('div', {
        class: 'nav-badge',
        title: 'This layout stays in place while content below is swapped by Switchback'
      }, '🔒 Persistent Layout')
    )
  );

  const main = h('main', {
    title: 'Only this content area changes when navigating - no full page reload'
  },
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
        props.title,
        h('span', { class: 'badge' }, 'PHP')
      ),
      h('p', {}, props.message),
      h('div', { class: 'stats' },
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.users),
          h('div', {}, 'Users')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.framework),
          h('div', {}, 'Backend')
        )
      ),
      h('div', { style: 'margin-top: 2rem;' },
        h('a', { href: '/users', 'data-swbk': '', class: 'btn' }, 'View Users')
      )
    )
  ),

  'Users/Index': (props) => Layout(
    h('div', {},
      h('h1', {}, 'Users'),
      h('div', { class: 'user-list' },
        ...props.users.map(user =>
          h('div', { class: 'user-card' },
            h('h3', {},
              h('a', { href: `/users/${user.id}`, 'data-swbk': '' }, user.name)
            ),
            h('p', {}, user.email)
          )
        )
      ),
      h('div', { style: 'margin-top: 2rem;' },
        h('a', { href: '/', 'data-swbk': '' }, '← Back to Home')
      )
    )
  ),

  'Users/Show': (props) => Layout(
    h('div', {},
      h('h1', {}, props.user.name),
      h('p', {}, h('strong', {}, 'Email: '), props.user.email),
      h('p', {}, h('strong', {}, 'Joined: '), props.user.joined),
      h('div', { style: 'margin-top: 2rem;' },
        h('a', { href: '/users', 'data-swbk': '' }, '← Back to Users')
      )
    )
  ),

  'About': (props) => Layout(
    h('div', {},
      h('h1', {}, 'About This Recipe'),
      h('p', {}, h('strong', {}, 'Version: '), props.version),
      h('p', {}, h('strong', {}, 'Backend: '), props.backend),
      h('h2', { style: 'margin-top: 1.5rem;' }, 'Features:'),
      h('ul', {},
        ...props.features.map(feature => h('li', {}, feature))
      ),
      h('div', { style: 'margin-top: 2rem;' },
        h('a', { href: '/', 'data-swbk': '' }, '← Back to Home')
      )
    )
  ),

  'Error': (props) => Layout(
    h('div', {},
      h('h1', {}, 'Error'),
      h('p', {}, props.message),
      h('div', { style: 'margin-top: 2rem;' },
        h('a', { href: '/', 'data-swbk': '' }, '← Back to Home')
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
    color: '#3498db',
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('PHP Recipe initialized!');
