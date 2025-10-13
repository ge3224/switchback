/**
 * Client-side app for C recipe
 * Bundles Switchback directly from source
 * Demonstrates OPTIMISTIC UPDATES - instant UI feedback before server confirms
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

// Global state for optimistic updates
const state = {
  todos: [],
  optimisticTodo: null,
  likes: {}
};

// Inject Matrix-style green terminal theme
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: 'Courier New', 'Consolas', monospace;
    background: #0a0e0a;
    color: #00ff41;
    line-height: 1.6;
  }

  nav {
    background: #0d1b0d;
    border-bottom: 2px solid #00ff41;
    padding: 1rem 2rem;
    position: sticky;
    top: 0;
    z-index: 100;
    box-shadow: 0 4px 12px rgba(0, 255, 65, 0.2);
  }

  nav .nav-content {
    max-width: 1200px;
    margin: 0 auto;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  nav a {
    color: #00ff41;
    text-decoration: none;
    margin-right: 1.5rem;
    padding: 0.5rem 1rem;
    border: 1px solid transparent;
    transition: all 0.2s;
    font-weight: bold;
    text-shadow: 0 0 5px rgba(0, 255, 65, 0.5);
  }

  nav a:hover {
    border-color: #00ff41;
    background: rgba(0, 255, 65, 0.1);
    box-shadow: 0 0 10px rgba(0, 255, 65, 0.3);
  }

  .nav-badge {
    background: #00ff41;
    color: #0a0e0a;
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
    background: linear-gradient(135deg, #0d1b0d 0%, #0a0e0a 100%);
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 2rem;
    text-align: center;
    box-shadow: 0 0 20px rgba(0, 255, 65, 0.2);
  }

  .demo-hint strong {
    color: #00ff41;
    font-size: 1.2rem;
    text-shadow: 0 0 10px rgba(0, 255, 65, 0.5);
  }

  .hint-flash {
    display: inline-block;
    color: #7fff00;
    animation: pulse 2s infinite;
  }

  @keyframes pulse {
    0%, 100% { opacity: 1; text-shadow: 0 0 10px rgba(127, 255, 0, 0.8); }
    50% { opacity: 0.6; text-shadow: 0 0 5px rgba(127, 255, 0, 0.4); }
  }

  h1 {
    font-size: 2.5rem;
    margin-bottom: 1rem;
    color: #00ff41;
    text-shadow: 0 0 20px rgba(0, 255, 65, 0.6);
  }

  h2 {
    font-size: 1.8rem;
    color: #00ff41;
    margin: 2rem 0 1rem;
    text-shadow: 0 0 15px rgba(0, 255, 65, 0.5);
  }

  .badge {
    background: #00ff41;
    color: #0a0e0a;
    padding: 0.3rem 0.8rem;
    border-radius: 6px;
    font-size: 1rem;
    margin-left: 1rem;
    display: inline-block;
    font-weight: bold;
  }

  .terminal-box {
    background: #0a0e0a;
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 1.5rem;
    margin: 1.5rem 0;
    font-family: 'Courier New', monospace;
    box-shadow: 0 0 15px rgba(0, 255, 65, 0.2);
  }

  .terminal-box::before {
    content: '$ ';
    color: #00ff41;
    font-weight: bold;
  }

  .terminal-box code {
    color: #7fff00;
  }

  .todo-input-section {
    background: #0d1b0d;
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 2rem;
    margin: 2rem 0;
    box-shadow: 0 0 20px rgba(0, 255, 65, 0.2);
  }

  .todo-input-form {
    display: flex;
    gap: 1rem;
    align-items: flex-start;
  }

  input, textarea {
    flex: 1;
    background: #0a0e0a;
    border: 1px solid #00ff41;
    color: #00ff41;
    padding: 0.75rem;
    border-radius: 4px;
    font-family: 'Courier New', monospace;
    transition: all 0.2s;
  }

  input:focus, textarea:focus {
    outline: none;
    border-color: #7fff00;
    box-shadow: 0 0 15px rgba(0, 255, 65, 0.3);
  }

  input::placeholder, textarea::placeholder {
    color: rgba(0, 255, 65, 0.4);
  }

  button, .btn {
    background: #00ff41;
    color: #0a0e0a;
    border: none;
    padding: 0.75rem 1.5rem;
    border-radius: 6px;
    cursor: pointer;
    font-weight: bold;
    font-family: 'Courier New', monospace;
    text-decoration: none;
    display: inline-block;
    transition: all 0.2s;
    box-shadow: 0 0 10px rgba(0, 255, 65, 0.4);
  }

  button:hover, .btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 20px rgba(0, 255, 65, 0.6);
  }

  button:disabled {
    opacity: 0.5;
    cursor: not-allowed;
    transform: none;
  }

  .todo-list {
    margin: 2rem 0;
  }

  .todo-item {
    background: #0d1b0d;
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 1.5rem;
    margin-bottom: 1rem;
    transition: all 0.3s;
    display: flex;
    justify-content: space-between;
    align-items: center;
    box-shadow: 0 0 10px rgba(0, 255, 65, 0.1);
  }

  .todo-item:hover {
    border-color: #7fff00;
    box-shadow: 0 0 20px rgba(0, 255, 65, 0.3);
    transform: translateX(5px);
  }

  .todo-item.optimistic {
    opacity: 0.6;
    border-style: dashed;
    animation: fadeIn 0.3s;
  }

  @keyframes fadeIn {
    from { opacity: 0; transform: translateY(-10px); }
    to { opacity: 0.6; transform: translateY(0); }
  }

  .todo-item.completed {
    opacity: 0.5;
    text-decoration: line-through;
  }

  .todo-content {
    flex: 1;
  }

  .todo-actions {
    display: flex;
    gap: 0.5rem;
  }

  .like-btn, .delete-btn {
    background: transparent;
    border: 1px solid #00ff41;
    color: #00ff41;
    padding: 0.5rem 1rem;
    font-size: 0.9rem;
    cursor: pointer;
    transition: all 0.2s;
  }

  .like-btn:hover, .delete-btn:hover {
    background: rgba(0, 255, 65, 0.1);
  }

  .like-btn.liked {
    background: #00ff41;
    color: #0a0e0a;
    font-weight: bold;
  }

  .stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
    margin: 2rem 0;
  }

  .stat-card {
    background: #0d1b0d;
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 1.5rem;
    text-align: center;
    box-shadow: 0 0 15px rgba(0, 255, 65, 0.2);
  }

  .stat-card strong {
    font-size: 2.5rem;
    color: #00ff41;
    display: block;
    margin-bottom: 0.5rem;
    text-shadow: 0 0 15px rgba(0, 255, 65, 0.6);
  }

  .stat-card div {
    color: #7fff00;
  }

  .info-box {
    background: #0d1b0d;
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 1.5rem;
    margin: 1.5rem 0;
    box-shadow: 0 0 15px rgba(0, 255, 65, 0.2);
  }

  .info-box strong {
    color: #7fff00;
  }

  a.back-link {
    color: #00ff41;
    text-decoration: none;
    display: inline-block;
    margin-top: 2rem;
    padding: 0.5rem 1rem;
    border: 1px solid #00ff41;
    border-radius: 4px;
    transition: all 0.2s;
  }

  a.back-link:hover {
    background: rgba(0, 255, 65, 0.1);
    box-shadow: 0 0 10px rgba(0, 255, 65, 0.3);
  }

  ul {
    margin-left: 2rem;
    margin-top: 1rem;
  }

  li {
    margin: 0.5rem 0;
    color: #7fff00;
  }

  code {
    background: #0a0e0a;
    padding: 0.2rem 0.5rem;
    border-radius: 3px;
    color: #7fff00;
  }
`;
document.head.appendChild(style);

// Layout component - PERSISTENT across page changes
function Layout(children) {
  const nav = h('nav', {},
    h('div', { class: 'nav-content' },
      h('div', {},
        h('a', { href: '/', 'data-swbk': '' }, '💚 Home'),
        h('a', { href: '/todos', 'data-swbk': '' }, '✓ Todos'),
        h('a', { href: '/about', 'data-swbk': '' }, 'ℹ️  About'),
      ),
      h('div', { class: 'nav-badge' }, 'OPTIMISTIC UI')
    )
  );

  const demoHint = h('div', { class: 'demo-hint' },
    h('strong', {}, '💚 C + Switchback Demo:'),
    ' This recipe demonstrates ',
    h('span', { class: 'hint-flash' }, 'OPTIMISTIC UPDATES'),
    ' - instant UI feedback!'
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
        'C Recipe',
        h('span', { class: 'badge' }, '💚 C99')
      ),
      h('div', { class: 'terminal-box' },
        'A blazingly fast C backend with ',
        h('code', {}, 'optimistic updates'),
        ' for instant UI feedback'
      ),
      h('div', { class: 'stats' },
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.todos),
          h('div', {}, 'Active Todos')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.totalLikes),
          h('div', {}, 'Total Likes')
        ),
        h('div', { class: 'stat-card' },
          h('strong', {}, props.stats.framework),
          h('div', {}, 'Backend')
        )
      ),
      h('div', { class: 'info-box' },
        h('strong', {}, '🚀 What are Optimistic Updates?'),
        h('p', { style: { marginTop: '0.5rem' } },
          'Optimistic updates make your app feel instant by updating the UI immediately, ',
          'before waiting for the server to respond. Try adding a todo to see it in action!'
        )
      ),
      h('div', { style: { marginTop: '2rem' } },
        h('a', { href: '/todos', 'data-swbk': '', class: 'btn' }, '✓ Try Optimistic Todos →')
      )
    )
  ),

  'Todos': (props) => {
    // Initialize state from props
    state.todos = props.todos || [];

    return Layout(
      h('div', {},
        h('h1', {}, '✓ Optimistic Todos'),
        h('div', { class: 'terminal-box' },
          'Add a todo and watch it appear ',
          h('code', {}, 'instantly'),
          ' with a dashed border. The border becomes solid once the server confirms!'
        ),

        // Todo input section
        h('div', { class: 'todo-input-section' },
          h('h2', { style: { marginTop: 0 } }, '> Add New Todo'),
          h('form', {
            class: 'todo-input-form',
            onSubmit: async (e) => {
              e.preventDefault();
              const input = e.target.querySelector('input');
              const text = input.value.trim();

              if (!text) return;

              // Create optimistic todo
              const optimisticId = `temp-${Date.now()}`;
              state.optimisticTodo = { id: optimisticId, text, completed: false, likes: 0 };

              // Update UI immediately
              renderTodos();
              input.value = '';
              input.focus();

              try {
                // Make request to server
                const response = await fetch('/api/todos', {
                  method: 'POST',
                  headers: {
                    'Content-Type': 'application/json',
                    'X-Switchback': '1'
                  },
                  body: JSON.stringify({ text })
                });

                const data = await response.json();

                // Replace optimistic todo with real one
                state.todos.push(data.todo);
                state.optimisticTodo = null;
                renderTodos();
              } catch (error) {
                console.error('Failed to add todo:', error);
                state.optimisticTodo = null;
                renderTodos();
                alert('Failed to add todo. Please try again.');
              }
            }
          },
            h('input', {
              type: 'text',
              placeholder: 'What needs to be done?',
              required: true,
              style: { flex: 1 }
            }),
            h('button', { type: 'submit' }, '💚 Add Todo')
          )
        ),

        // Todos list
        h('div', { class: 'todo-list', id: 'todo-list' },
          // Will be filled by renderTodos()
        ),

        h('a', { href: '/', 'data-swbk': '', class: 'back-link' }, '← Back to Home')
      )
    );
  },

  'About': (props) => Layout(
    h('div', {},
      h('h1', {}, 'ℹ️  About This Recipe'),
      h('div', { class: 'terminal-box' },
        `C Recipe v${props.version}`
      ),
      h('p', { style: { margin: '1.5rem 0', fontSize: '1.1rem' } },
        h('strong', { style: { color: '#7fff00' } }, '🔧 Backend: '),
        props.backend
      ),
      h('h2', {}, 'Key Features:'),
      h('ul', {},
        ...props.features.map(feature => h('li', {}, `💚 ${feature}`))
      ),
      h('h2', {}, 'What are Optimistic Updates?'),
      h('div', { class: 'info-box' },
        h('p', {},
          'Optimistic updates are a UI pattern where you update the interface ',
          h('strong', {}, 'immediately'),
          ' when the user performs an action, without waiting for the server to respond.'
        ),
        h('p', { style: { marginTop: '1rem' } },
          'Benefits:'
        ),
        h('ul', {},
          h('li', {}, '💚 Instant feedback - feels lightning fast'),
          h('li', {}, '🎯 Better UX - no waiting for network requests'),
          h('li', {}, '🔄 Graceful rollback - revert if server fails'),
          h('li', {}, '📱 Essential for mobile apps with spotty connections')
        )
      ),
      h('h2', {}, 'How It Works:'),
      h('div', { class: 'info-box' },
        h('ol', { style: { marginLeft: '2rem' } },
          h('li', {}, 'User adds a todo'),
          h('li', {}, 'UI updates ',  h('code', {}, 'immediately'), ' (optimistic todo shown with dashed border)'),
          h('li', {}, 'Request sent to C backend'),
          h('li', {}, 'Server processes and responds'),
          h('li', {}, 'UI replaces optimistic todo with confirmed one (solid border)')
        ),
        h('p', { style: { marginTop: '1rem' } },
          'If the server fails, the optimistic update is rolled back and an error is shown.'
        )
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

// Function to render todos list
function renderTodos() {
  const container = document.getElementById('todo-list');
  if (!container) return;

  container.innerHTML = '';

  // Show optimistic todo first
  if (state.optimisticTodo) {
    const todo = state.optimisticTodo;
    const todoEl = h('div', { class: 'todo-item optimistic' },
      h('div', { class: 'todo-content' },
        h('div', { style: { fontSize: '1.1rem', marginBottom: '0.5rem' } }, todo.text),
        h('div', { style: { fontSize: '0.9rem', color: '#7fff00' } },
          `👍 ${todo.likes} likes • ⏳ Adding...`
        )
      )
    );
    container.appendChild(todoEl);
  }

  // Show confirmed todos
  state.todos.forEach((todo) => {
    const todoEl = h('div', {
      class: `todo-item${todo.completed ? ' completed' : ''}`,
      'data-todo-id': todo.id
    },
      h('div', { class: 'todo-content' },
        h('div', { style: { fontSize: '1.1rem', marginBottom: '0.5rem' } }, todo.text),
        h('div', { style: { fontSize: '0.9rem', color: '#7fff00' } },
          `👍 ${todo.likes} likes`
        )
      ),
      h('div', { class: 'todo-actions' },
        h('button', {
          class: `like-btn${state.likes[todo.id] ? ' liked' : ''}`,
          onClick: () => handleLike(todo.id)
        }, state.likes[todo.id] ? '❤️ Liked' : '👍 Like'),
        h('button', {
          class: 'delete-btn',
          onClick: () => handleDelete(todo.id)
        }, '🗑️ Delete')
      )
    );
    container.appendChild(todoEl);
  });

  if (state.todos.length === 0 && !state.optimisticTodo) {
    const emptyEl = h('div', { class: 'info-box', style: { textAlign: 'center' } },
      h('p', {}, '📝 No todos yet. Add one above to get started!')
    );
    container.appendChild(emptyEl);
  }
}

// Handle like button (optimistic update)
async function handleLike(todoId) {
  // Optimistically update UI
  const currentlyLiked = state.likes[todoId];
  state.likes[todoId] = !currentlyLiked;

  // Find todo and update likes count optimistically
  const todo = state.todos.find(t => t.id === todoId);
  if (todo) {
    const originalLikes = todo.likes;
    todo.likes += currentlyLiked ? -1 : 1;
    renderTodos();

    try {
      // Send request to server
      const response = await fetch(`/api/todos/${todoId}/like`, {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'X-Switchback': '1'
        },
        body: JSON.stringify({ liked: !currentlyLiked })
      });

      const data = await response.json();

      // Update with server response
      todo.likes = data.likes;
      renderTodos();
    } catch (error) {
      console.error('Failed to like todo:', error);
      // Rollback on error
      state.likes[todoId] = currentlyLiked;
      todo.likes = originalLikes;
      renderTodos();
      alert('Failed to update like. Please try again.');
    }
  }
}

// Handle delete button (optimistic update)
async function handleDelete(todoId) {
  if (!confirm('Delete this todo?')) return;

  // Store todo for potential rollback
  const todoIndex = state.todos.findIndex(t => t.id === todoId);
  const deletedTodo = state.todos[todoIndex];

  // Optimistically remove from UI
  state.todos.splice(todoIndex, 1);
  renderTodos();

  try {
    // Send delete request
    await fetch(`/api/todos/${todoId}`, {
      method: 'DELETE',
      headers: {
        'X-Switchback': '1'
      }
    });
  } catch (error) {
    console.error('Failed to delete todo:', error);
    // Rollback on error
    state.todos.splice(todoIndex, 0, deletedTodo);
    renderTodos();
    alert('Failed to delete todo. Please try again.');
  }
}

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

    // Initial render for todos page
    if (props.todos) {
      setTimeout(renderTodos, 0);
    }
  },

  initialPage: window.initialPage,

  progress: {
    delay: 250,
    color: '#00ff41',  // Green progress bar
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('💚 C Recipe initialized with optimistic updates!');
