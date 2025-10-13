/**
 * Example Switchback app with just-jsx and simple-state
 *
 * Demonstrates a complete SPA setup with:
 * - Page components using JSX
 * - Shared state for flash messages
 * - Automatic link/form interception
 * - Progress indicators
 */

import { newSwitchback, visit, page, reload } from '../src/index';
import { createDomElement as h } from '../../just-jsx/main/src/index';
import { newSimpleState } from '../../simple-state/main/src/index';

// ============================================================================
// Shared State
// ============================================================================

interface FlashMessage {
  type: 'success' | 'error' | 'info';
  message: string;
}

const flashState = newSimpleState<FlashMessage | null>(null);

// ============================================================================
// Components
// ============================================================================

const Layout = ({ children }: { children: any }) => (
  <div class="app">
    <nav>
      <a href="/">Home</a>
      <a href="/users">Users</a>
      <a href="/about">About</a>
    </nav>

    <FlashMessages />

    <main>
      {children}
    </main>
  </div>
);

const FlashMessages = () => {
  const flash = flashState.get();

  if (!flash) return null;

  return (
    <div class={`flash flash-${flash.type}`}>
      {flash.message}
      <button onClick={() => flashState.set(null)}>×</button>
    </div>
  );
};

const Home = ({ title, stats }: any) => (
  <Layout>
    <h1>{title}</h1>
    <div class="stats">
      <div>Users: {stats.users}</div>
      <div>Posts: {stats.posts}</div>
    </div>
    <a href="/users">View all users</a>
  </Layout>
);

const UserList = ({ users }: any) => (
  <Layout>
    <h1>Users</h1>
    <div class="user-list">
      {users.map((user: any) => (
        <div class="user-card" key={user.id}>
          <h3>
            <a href={`/users/${user.id}`}>{user.name}</a>
          </h3>
          <p>{user.email}</p>
        </div>
      ))}
    </div>
    <a href="/users/new">Create New User</a>
  </Layout>
);

const UserShow = ({ user }: any) => (
  <Layout>
    <h1>{user.name}</h1>
    <p><strong>Email:</strong> {user.email}</p>
    <p><strong>Joined:</strong> {user.joined}</p>

    <div class="actions">
      <a href={`/users/${user.id}/edit`}>Edit</a>
      <button
        onClick={() => {
          if (confirm('Delete this user?')) {
            visit(`/users/${user.id}`, { method: 'delete' });
          }
        }}
      >
        Delete
      </button>
    </div>

    <a href="/users">Back to list</a>
  </Layout>
);

const UserForm = ({ user, errors }: any) => (
  <Layout>
    <h1>{user ? 'Edit User' : 'New User'}</h1>

    <form action={user ? `/users/${user.id}` : '/users'} method="post">
      <div class="form-group">
        <label for="name">Name:</label>
        <input
          id="name"
          name="name"
          value={user?.name || ''}
          class={errors?.name ? 'error' : ''}
        />
        {errors?.name && <span class="error-message">{errors.name}</span>}
      </div>

      <div class="form-group">
        <label for="email">Email:</label>
        <input
          id="email"
          name="email"
          type="email"
          value={user?.email || ''}
          class={errors?.email ? 'error' : ''}
        />
        {errors?.email && <span class="error-message">{errors.email}</span>}
      </div>

      <div class="form-actions">
        <button type="submit">Save</button>
        <a href={user ? `/users/${user.id}` : '/users'}>Cancel</a>
      </div>
    </form>
  </Layout>
);

const About = ({ version, features }: any) => (
  <Layout>
    <h1>About Switchback</h1>
    <p><strong>Version:</strong> {version}</p>
    <h2>Features:</h2>
    <ul>
      {features.map((feature: string) => (
        <li key={feature}>{feature}</li>
      ))}
    </ul>
    <a href="/">Home</a>
  </Layout>
);

// ============================================================================
// Page Registry
// ============================================================================

const pages: Record<string, (props: any) => any> = {
  'Home': Home,
  'Users/Index': UserList,
  'Users/Show': UserShow,
  'Users/Form': UserForm,
  'About': About,
};

// ============================================================================
// App Initialization
// ============================================================================

const app = newSwitchback({
  resolve: (name: string) => {
    const component = pages[name];
    if (!component) {
      throw new Error(`Component "${name}" not found`);
    }
    return component;
  },

  setup: ({ el, App, props }) => {
    // Handle flash messages
    if (props.flash) {
      flashState.set(props.flash);
      // Clear after 5 seconds
      setTimeout(() => flashState.set(null), 5000);
    }

    // Subscribe to flash state changes
    let flashSubscription: number | null = null;

    const render = () => {
      el.innerHTML = '';
      el.appendChild(App(props));
    };

    // Initial render
    render();

    // Re-render when flash messages change
    if (flashSubscription !== null) {
      flashState.unsubscribe(flashSubscription);
    }
    flashSubscription = flashState.subscribe(() => {
      render();
    });
  },

  initialPage: (window as any).initialPage,

  progress: {
    delay: 250,
    color: '#3b82f6',
    includeCSS: true,
    showSpinner: true,
  },
});

// ============================================================================
// Global exports for debugging
// ============================================================================

(window as any).switchback = { visit, page, reload, flashState };
