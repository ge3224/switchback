/**
 * Switchback Example App
 *
 * Demonstrates a complete SPA using Switchback with:
 * - Multiple page components
 * - Link and form interception
 * - Flash messages
 * - Client-side only demo (no actual backend)
 */

import { newSwitchback, type Switchback } from '../src/index';

// ============================================================================
// Mock Backend - Simulates server responses
// ============================================================================

interface User {
  id: number;
  name: string;
  email: string;
  joined: string;
}

const mockUsers: User[] = [
  { id: 1, name: 'Alice Johnson', email: 'alice@example.com', joined: '2024-01-15' },
  { id: 2, name: 'Bob Smith', email: 'bob@example.com', joined: '2024-02-20' },
  { id: 3, name: 'Charlie Brown', email: 'charlie@example.com', joined: '2024-03-10' },
];

// Mock server responses
async function mockFetch(url: string, options: any = {}): Promise<any> {
  await new Promise(resolve => setTimeout(resolve, 300)); // Simulate network delay

  const path = new URL(url, window.location.origin).pathname;

  // Home page
  if (path === '/' || path === '') {
    return {
      component: 'Home',
      props: {
        title: 'Welcome to Switchback',
        stats: { users: mockUsers.length, posts: 42 },
      },
      url: '/',
    };
  }

  // Users list
  if (path === '/users') {
    return {
      component: 'Users/Index',
      props: { users: mockUsers },
      url: '/users',
    };
  }

  // User detail
  const userMatch = path.match(/^\/users\/(\d+)$/);
  if (userMatch) {
    const userId = parseInt(userMatch[1]);
    const user = mockUsers.find(u => u.id === userId);

    if (user) {
      return {
        component: 'Users/Show',
        props: { user },
        url: `/users/${userId}`,
      };
    }
  }

  // About page
  if (path === '/about') {
    return {
      component: 'About',
      props: {
        version: '0.1.0',
        features: [
          'Zero dependencies',
          'Inertia-inspired architecture',
          'Works with any backend',
          'Automatic link interception',
          'Form handling',
          'Scroll restoration',
          'Flash messages',
        ],
      },
      url: '/about',
    };
  }

  throw new Error('Page not found');
}

// ============================================================================
// Simple State for Flash Messages
// ============================================================================

let flashMessage: { type: string; message: string } | null = null;
let flashSubscribers: Array<() => void> = [];

function setFlash(flash: { type: string; message: string } | null) {
  flashMessage = flash;
  flashSubscribers.forEach(cb => cb());
}

function subscribeFlash(callback: () => void) {
  flashSubscribers.push(callback);
  return () => {
    flashSubscribers = flashSubscribers.filter(cb => cb !== callback);
  };
}

// ============================================================================
// Components (vanilla JS createElement)
// ============================================================================

function h(tag: string, props: any = {}, ...children: any[]): HTMLElement {
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

function Layout(children: HTMLElement): HTMLElement {
  const nav = h('nav', {},
    h('a', { href: '/', class: location.pathname === '/' ? 'active' : '' }, 'Home'),
    h('a', { href: '/users', class: location.pathname.startsWith('/users') ? 'active' : '' }, 'Users'),
    h('a', { href: '/about', class: location.pathname === '/about' ? 'active' : '' }, 'About'),
  );

  const main = h('main', {}, children);

  const container = h('div', {});
  container.appendChild(nav);

  if (flashMessage) {
    const flash = h('div', { class: `flash flash-${flashMessage.type}`, style: 'margin: 0 auto; max-width: 900px; margin-top: 1rem;' },
      h('span', {}, flashMessage.message),
      h('button', {
        onClick: () => setFlash(null)
      }, '×')
    );
    container.appendChild(flash);
  }

  container.appendChild(main);
  return container;
}

// Home Page
function HomePage(props: any): HTMLElement {
  return Layout(
    h('div', {},
      h('h1', {}, props.title),
      h('p', {}, 'This is a demo of Switchback - an Inertia-inspired SPA library for vanilla TypeScript.'),
      h('div', { class: 'stats' },
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.users),
          h('div', {}, 'Users')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.posts),
          h('div', {}, 'Posts')
        )
      ),
      h('p', { style: 'margin-top: 2rem;' },
        'Click around the app - notice how navigation is instant with no full page reloads!'
      ),
      h('div', { style: 'margin-top: 1rem;' },
        h('a', { href: '/users', class: 'btn' }, 'View All Users')
      )
    )
  );
}

// Users List Page
function UsersIndexPage(props: any): HTMLElement {
  const userCards = props.users.map((user: User) =>
    h('div', { class: 'user-card' },
      h('h3', {},
        h('a', { href: `/users/${user.id}` }, user.name)
      ),
      h('p', {}, user.email)
    )
  );

  return Layout(
    h('div', {},
      h('h1', {}, 'Users'),
      h('div', { class: 'user-list' }, ...userCards),
      h('div', { style: 'margin-top: 2rem;' },
        h('a', { href: '/' }, '← Back to Home')
      )
    )
  );
}

// User Show Page
function UserShowPage(props: any): HTMLElement {
  return Layout(
    h('div', {},
      h('h1', {}, props.user.name),
      h('p', {}, h('strong', {}, 'Email: '), props.user.email),
      h('p', {}, h('strong', {}, 'Joined: '), props.user.joined),
      h('div', { class: 'actions' },
        h('button', {
          onClick: () => {
            setFlash({ type: 'info', message: 'Edit functionality would go here in a real app!' });
          }
        }, 'Edit User'),
        h('button', {
          class: 'btn-danger',
          onClick: () => {
            if (confirm('Delete this user?')) {
              setFlash({ type: 'success', message: `${props.user.name} deleted successfully!` });
              app.visit('/users');
            }
          }
        }, 'Delete User')
      ),
      h('div', { style: 'margin-top: 2rem;' },
        h('a', { href: '/users' }, '← Back to Users')
      )
    )
  );
}

// About Page
function AboutPage(props: any): HTMLElement {
  const features = props.features.map((feature: string) =>
    h('li', {}, feature)
  );

  return Layout(
    h('div', {},
      h('h1', {}, 'About Switchback'),
      h('p', {}, h('strong', {}, 'Version: '), props.version),
      h('h2', {}, 'Features:'),
      h('ul', {}, ...features),
      h('h2', {}, 'How it Works'),
      h('p', {},
        'Switchback intercepts link clicks and form submissions, fetching page data from ',
        'your server as JSON instead of doing full page reloads. This gives you the speed ',
        'of a SPA with the simplicity of traditional server-side routing.'
      ),
      h('p', {},
        'In a real application, your backend would detect the ',
        h('code', {}, 'X-Switchback'),
        ' header and return JSON instead of HTML.'
      ),
      h('div', { style: 'margin-top: 2rem;' },
        h('a', { href: '/' }, '← Back to Home')
      )
    )
  );
}

// ============================================================================
// Page Registry
// ============================================================================

const pages: Record<string, (props: any) => HTMLElement> = {
  'Home': HomePage,
  'Users/Index': UsersIndexPage,
  'Users/Show': UserShowPage,
  'About': AboutPage,
};

// ============================================================================
// App Initialization
// ============================================================================

// App instance (initialized below)
let app: Switchback;

// Intercept fetch to use mock backend
const originalFetch = window.fetch;
window.fetch = async (input: RequestInfo | URL, init?: RequestInit) => {
  const url = typeof input === 'string' ? input : input instanceof URL ? input.href : input.url;
  const headers = new Headers(init?.headers);

  if (headers.get('X-Switchback')) {
    try {
      const response = await mockFetch(url, init);
      return new Response(JSON.stringify(response), {
        status: 200,
        headers: { 'Content-Type': 'application/json' },
      });
    } catch (error) {
      return new Response(JSON.stringify({ error: (error as Error).message }), {
        status: 404,
        headers: { 'Content-Type': 'application/json' },
      });
    }
  }

  return originalFetch(input, init);
};

// Initialize app
(async () => {
  const initialPage = await mockFetch(window.location.pathname);

  app = newSwitchback({
    resolve: (name: string) => {
      const component = pages[name];
      if (!component) {
        throw new Error(`Component "${name}" not found`);
      }
      return component;
    },

    setup: ({ el, App, props }) => {
      // Handle flash messages from props
      if (props.flash) {
        setFlash(props.flash);
        setTimeout(() => setFlash(null), 5000);
      }

      // Render function
      const render = () => {
        el.innerHTML = '';
        el.appendChild(App(props));
      };

      // Subscribe to flash changes
      const unsubscribe = subscribeFlash(render);

      // Initial render
      render();
    },

    initialPage,
  });

  console.log('Switchback demo app initialized!');
  console.log('Try navigating around - notice the instant navigation without page reloads.');
})();
