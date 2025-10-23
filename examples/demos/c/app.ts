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

  header {
    background: #0d1b0d;
    border-bottom: 2px solid #00ff41;
    padding: 1rem 2rem;
    text-align: center;
    box-shadow: 0 4px 12px rgba(0, 255, 65, 0.2);
  }

  header h1 {
    margin: 0;
    font-size: 2rem;
  }

  .header-badge {
    background: #00ff41;
    color: #0a0e0a;
    padding: 0.4rem 0.8rem;
    border-radius: 4px;
    font-size: 0.85rem;
    font-weight: bold;
    margin-left: 1rem;
  }

  main {
    max-width: 1200px;
    margin: 1.5rem auto;
    padding: 0 2rem;
  }

  .demo-hint {
    background: linear-gradient(135deg, #0d1b0d 0%, #0a0e0a 100%);
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 1rem;
    margin-bottom: 1.5rem;
    text-align: center;
    box-shadow: 0 0 20px rgba(0, 255, 65, 0.2);
    font-size: 0.95rem;
  }

  .demo-hint strong {
    color: #00ff41;
    font-size: 1rem;
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
    grid-template-columns: repeat(3, 1fr);
    gap: 1rem;
    margin: 0 0 1.5rem 0;
  }

  .stat-card {
    background: #0d1b0d;
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 1rem;
    text-align: center;
    box-shadow: 0 0 15px rgba(0, 255, 65, 0.2);
  }

  .stat-card strong {
    font-size: 1.8rem;
    color: #00ff41;
    display: block;
    margin-bottom: 0.25rem;
    text-shadow: 0 0 15px rgba(0, 255, 65, 0.6);
  }

  .stat-card div {
    color: #7fff00;
    font-size: 0.9rem;
  }

  .info-box {
    background: #0d1b0d;
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 1rem;
    margin: 0 0 1.5rem 0;
    box-shadow: 0 0 15px rgba(0, 255, 65, 0.2);
  }

  .info-box h2 {
    font-size: 1.3rem;
    margin-bottom: 0.5rem;
  }

  .info-box p {
    font-size: 0.95rem;
    line-height: 1.5;
  }

  .info-box strong {
    color: #7fff00;
  }

  .info-box ul {
    margin-top: 0.5rem;
    margin-left: 1.5rem;
  }

  .info-box li {
    font-size: 0.9rem;
    margin: 0.25rem 0;
  }

  .section {
    margin: 0 0 1.5rem 0;
  }

  .section:last-child {
    margin-bottom: 2rem;
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

  .modal-overlay {
    position: fixed;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    background: rgba(0, 0, 0, 0.8);
    display: flex;
    align-items: center;
    justify-content: center;
    z-index: 1000;
    animation: fadeInOverlay 0.2s;
  }

  @keyframes fadeInOverlay {
    from { opacity: 0; }
    to { opacity: 1; }
  }

  .modal {
    background: #0d1b0d;
    border: 2px solid #00ff41;
    border-radius: 8px;
    padding: 2rem;
    max-width: 400px;
    box-shadow: 0 0 30px rgba(0, 255, 65, 0.4);
    animation: scaleIn 0.2s;
  }

  @keyframes scaleIn {
    from { transform: scale(0.9); opacity: 0; }
    to { transform: scale(1); opacity: 1; }
  }

  .modal h3 {
    margin: 0 0 1rem 0;
    color: #00ff41;
    font-size: 1.3rem;
    text-shadow: 0 0 10px rgba(0, 255, 65, 0.5);
  }

  .modal p {
    margin: 0 0 1.5rem 0;
    color: #7fff00;
  }

  .modal-actions {
    display: flex;
    gap: 1rem;
    justify-content: flex-end;
  }

  .modal-actions button {
    min-width: 100px;
  }

  .btn-secondary {
    background: transparent;
    border: 1px solid #00ff41;
    color: #00ff41;
  }

  .btn-secondary:hover {
    background: rgba(0, 255, 65, 0.1);
  }

  .btn-danger {
    background: #ff4444;
    box-shadow: 0 0 10px rgba(255, 68, 68, 0.4);
  }

  .btn-danger:hover {
    background: #ff6666;
    box-shadow: 0 4px 20px rgba(255, 68, 68, 0.6);
  }
`;
document.head.appendChild(style);

// Show confirmation modal
function showConfirmModal(message) {
  return new Promise((resolve) => {
    const modal = h('div', { class: 'modal-overlay' },
      h('div', { class: 'modal' },
        h('h3', {}, '⚠️  Confirm Delete'),
        h('p', {}, message),
        h('div', { class: 'modal-actions' },
          h('button', {
            class: 'btn btn-secondary',
            onClick: () => {
              document.body.removeChild(modal);
              resolve(false);
            }
          }, 'Cancel'),
          h('button', {
            class: 'btn btn-danger',
            onClick: () => {
              document.body.removeChild(modal);
              resolve(true);
            }
          }, 'Delete')
        )
      )
    );

    // Close on overlay click
    modal.addEventListener('click', (e) => {
      if (e.target === modal) {
        document.body.removeChild(modal);
        resolve(false);
      }
    });

    // Close on Escape key
    const handleEscape = (e) => {
      if (e.key === 'Escape') {
        document.body.removeChild(modal);
        document.removeEventListener('keydown', handleEscape);
        resolve(false);
      }
    };
    document.addEventListener('keydown', handleEscape);

    document.body.appendChild(modal);
  });
}

// Show error modal
function showErrorModal(message) {
  return new Promise((resolve) => {
    const modal = h('div', { class: 'modal-overlay' },
      h('div', { class: 'modal' },
        h('h3', {}, '❌ Error'),
        h('p', {}, message),
        h('div', { class: 'modal-actions' },
          h('button', {
            class: 'btn',
            onClick: () => {
              document.body.removeChild(modal);
              resolve();
            }
          }, 'OK')
        )
      )
    );

    // Close on overlay click
    modal.addEventListener('click', (e) => {
      if (e.target === modal) {
        document.body.removeChild(modal);
        resolve();
      }
    });

    // Close on Escape key
    const handleEscape = (e) => {
      if (e.key === 'Escape') {
        document.body.removeChild(modal);
        document.removeEventListener('keydown', handleEscape);
        resolve();
      }
    };
    document.addEventListener('keydown', handleEscape);

    document.body.appendChild(modal);
  });
}

// Layout component
function Layout(children) {
  const header = h('header', {},
    h('h1', {},
      '💚 C + Switchback Demo',
      h('span', { class: 'header-badge' }, 'OPTIMISTIC UI')
    )
  );

  const demoHint = h('div', { class: 'demo-hint' },
    h('strong', {}, 'This recipe demonstrates '),
    h('span', { class: 'hint-flash' }, 'OPTIMISTIC UPDATES'),
    ' - instant UI feedback before server confirms!'
  );

  const main = h('main', {},
    demoHint,
    children
  );

  const container = h('div', {});
  container.appendChild(header);
  container.appendChild(main);
  return container;
}

// Single Page Component
const pages = {
  'Home': (props) => {
    // Initialize state from props
    state.todos = props.todos || [];

    return Layout(
      h('div', {},
        // Stats section
        h('div', { class: 'section' },
          h('div', { class: 'stats' },
            h('div', { class: 'stat-card' },
              h('strong', {}, props.stats?.todos || 0),
              h('div', {}, 'Active Todos')
            ),
            h('div', { class: 'stat-card' },
              h('strong', {}, props.stats?.totalLikes || 0),
              h('div', {}, 'Total Likes')
            ),
            h('div', { class: 'stat-card' },
              h('strong', {}, props.stats?.framework || 'C99'),
              h('div', {}, 'Backend')
            )
          )
        ),

        // What are Optimistic Updates section
        h('div', { class: 'section' },
          h('div', { class: 'info-box' },
            h('h2', { style: { marginTop: 0 } }, '🚀 What are Optimistic Updates?'),
            h('p', { style: { marginTop: '0.5rem', marginBottom: 0 } },
              'Updates the UI ',
              h('strong', {}, 'instantly'),
              ' before the server responds. Watch the dashed border become solid when confirmed!'
            )
          )
        ),

        // Todo input section
        h('div', { class: 'section' },
          h('div', { class: 'todo-input-section' },
            h('h2', { style: { marginTop: 0, marginBottom: '1rem' } }, '✓ Try It: Add a Todo'),
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
                  showErrorModal('Failed to add todo. Please try again.');
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
          )
        ),

        // Todos list
        h('div', { class: 'section' },
          h('div', { class: 'todo-list', id: 'todo-list' },
            // Will be filled by renderTodos()
          )
        ),

        // How it works section
        h('div', { class: 'section' },
          h('div', { class: 'info-box' },
            h('h2', { style: { marginTop: 0 } }, '⚙️ How It Works'),
            h('ol', { style: { marginLeft: '2rem', marginTop: '1rem' } },
              h('li', {}, 'User adds a todo'),
              h('li', {}, 'UI updates ', h('code', {}, 'immediately'), ' (optimistic todo shown with dashed border)'),
              h('li', {}, 'Request sent to C backend'),
              h('li', {}, 'Server processes and responds'),
              h('li', {}, 'UI replaces optimistic todo with confirmed one (solid border)')
            ),
            h('p', { style: { marginTop: '1rem' } },
              'If the server fails, the optimistic update is rolled back and an error is shown.'
            )
          )
        )
      )
    );
  },

  'Error': (props) => Layout(
    h('div', {},
      h('h1', {}, '❌ Error'),
      h('div', { class: 'terminal-box' },
        `Error: ${props.message}`
      )
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
      showErrorModal('Failed to update like. Please try again.');
    }
  }
}

// Handle delete button (optimistic update)
async function handleDelete(todoId) {
  const todo = state.todos.find(t => t.id === todoId);
  if (!todo) return;

  // Show custom confirmation modal
  const confirmed = await showConfirmModal(`Are you sure you want to delete "${todo.text}"?`);
  if (!confirmed) return;

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

    // Show error modal instead of alert
    await showErrorModal('Failed to delete todo. Please try again.');
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
