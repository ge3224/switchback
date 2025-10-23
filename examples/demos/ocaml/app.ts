/**
 * OCaml + Switchback Demo
 * Showcases Switchback's full feature set with OCaml's type-safe state machine
 *
 * Switchback Features Demonstrated:
 * 1. app.visit() - Instant SPA navigation (no page reload!)
 * 2. method: 'post' - Form submissions via POST with Switchback
 * 3. data: {...} - Sending form data to server
 * 4. preserveScroll: true - Maintain scroll position during navigation
 * 5. onStart - Lifecycle hook for loading states
 * 6. onSuccess - Success callback with page data
 * 7. onError - Error handling with server validation messages
 * 8. onFinish - Cleanup after request completes
 * 9. Optimistic updates - Instant UI feedback before server confirms
 * 10. Progress indicators - Automatic loading bar during navigation
 * 11. app.reload() - Refresh current component without navigation
 *
 * This demonstrates how Switchback's minimal API enables complex UX patterns
 * while keeping the backend (OCaml) as the source of truth for business logic.
 */

import { newSwitchback } from '../../../src/index.ts';

// Simple JSX-like helper
function h(tag: string, props: any = {}, ...children: any[]): HTMLElement {
  const element = document.createElement(tag);

  Object.keys(props || {}).forEach(key => {
    if (key === 'key') {
      // Skip key prop (used for list rendering)
    } else if (key.startsWith('on')) {
      element.addEventListener(key.slice(2).toLowerCase(), props[key]);
    } else if (key === 'class' || key === 'className') {
      element.className = props[key];
    } else if (key === 'style' && typeof props[key] === 'string') {
      element.setAttribute('style', props[key]);
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

// ============================================================================
// TYPES
// ============================================================================

interface TaskState {
  status: string;
  label: string;
  reviewer?: string;
  submitted_at?: number;
  approved_at?: number;
  published_at?: number;
  rejected_at?: number;
  reason?: string;
  url?: string;
}

interface Task {
  id: number;
  title: string;
  description: string;
  priority: 'low' | 'medium' | 'high' | 'critical';
  state: TaskState;
  created_at: number;
}

interface TaskListProps {
  tasks: Task[];
}

interface TaskDetailProps {
  task: Task;
  availableActions: string[];
}

// ============================================================================
// GLOBAL STATE
// ============================================================================

const state = {
  tasks: [] as Task[],
  currentTask: null as Task | null,
  optimisticTransition: null as { taskId: number; newState: TaskState } | null,
  isTransitioning: false,
  showTransitionForm: null as { taskId: number; action: string } | null,
  toastMessage: null as { message: string; type: 'success' | 'error' } | null,
};

// ============================================================================
// API FUNCTIONS
// ============================================================================

async function fetchTasks(): Promise<Task[]> {
  const response = await fetch('/api/tasks');
  const data = await response.json();
  return data.tasks;
}

async function createTask(title: string, description: string, priority: string): Promise<Task> {
  const response = await fetch('/api/tasks', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ title, description, priority }),
  });
  const data = await response.json();
  return data.task;
}

async function transitionTask(
  taskId: number,
  action: string,
  reviewer: string,
  reason?: string,
  url?: string
): Promise<{ success: boolean; task?: Task; error?: string }> {
  const response = await fetch(`/api/tasks/${taskId}/transition`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ action, reviewer, reason, url }),
  });
  return response.json();
}

// ============================================================================
// UI COMPONENTS
// ============================================================================

// Layout wrapper with simple header
const Layout = (children: any[]) => {
  return h('div', { style: 'min-height: 100vh; background: #1a1a2e; color: #eee;' }, [
    // Toast notification (global)
    Toast(),

    // Transition form modal (global)
    state.showTransitionForm && TransitionForm(state.showTransitionForm.taskId, state.showTransitionForm.action),

    // Header
    h('header', {
      style: 'background: linear-gradient(135deg, #ff6b35 0%, #f7931e 100%); padding: 1.5rem; box-shadow: 0 4px 6px rgba(0,0,0,0.3);'
    }, [
      h('div', { style: 'max-width: 1200px; margin: 0 auto;' }, [
        h('h1', { style: 'margin: 0; font-size: 2rem; color: white;' }, '🐫 OCaml Type-Safe Workflow'),
        h('p', { style: 'margin: 0.5rem 0 0 0; color: rgba(255,255,255,0.9); font-size: 0.9rem;' },
          'Pattern matching + Algebraic data types + Switchback instant navigation'),
      ]),
    ]),

    // Main content
    h('main', {
      style: 'max-width: 1200px; margin: 0 auto; padding: 2rem;'
    }, children),

    // Footer
    h('footer', {
      style: 'text-align: center; padding: 2rem; color: #999; border-top: 1px solid #333;'
    }, [
      h('p', {}, 'Powered by OCaml + Switchback'),
    ]),
  ]);
};

// Priority badge
const PriorityBadge = (priority: string) => {
  const colors = {
    low: '#4caf50',
    medium: '#ff9800',
    high: '#f44336',
    critical: '#9c27b0',
  };
  return h('span', {
    style: `display: inline-block; padding: 0.25rem 0.75rem; border-radius: 12px; background: ${colors[priority as keyof typeof colors] || '#999'}; color: white; font-size: 0.85rem; font-weight: bold; text-transform: uppercase;`
  }, priority);
};

// State badge
const StateBadge = (state: TaskState) => {
  const colors = {
    draft: '#607d8b',
    in_review: '#2196f3',
    approved: '#4caf50',
    published: '#9c27b0',
    rejected: '#f44336',
  };
  return h('span', {
    style: `display: inline-block; padding: 0.25rem 0.75rem; border-radius: 12px; background: ${colors[state.status as keyof typeof colors] || '#999'}; color: white; font-size: 0.85rem; font-weight: bold;`
  }, state.label);
};

// Toast notification
const Toast = () => {
  if (!state.toastMessage) return null;

  const { message, type } = state.toastMessage;
  const bgColor = type === 'success' ? '#4caf50' : '#f44336';

  // Auto-dismiss after 3 seconds
  setTimeout(() => {
    state.toastMessage = null;
    app.reload();
  }, 3000);

  return h('div', {
    style: `position: fixed; top: 1rem; right: 1rem; background: ${bgColor}; color: white; padding: 1rem 1.5rem; border-radius: 8px; box-shadow: 0 4px 12px rgba(0,0,0,0.3); z-index: 9999; animation: slideIn 0.3s ease-out;`
  }, [
    h('style', {}, `
      @keyframes slideIn {
        from { transform: translateX(100%); opacity: 0; }
        to { transform: translateX(0); opacity: 1; }
      }
    `),
    message
  ]);
};

// Helper function to show toast
function showToast(message: string, type: 'success' | 'error' = 'success') {
  state.toastMessage = { message, type };
  app.reload();
}

// Transition form component
const TransitionForm = (taskId: number, action: string) => {
  const actionLabels: Record<string, string> = {
    review: 'Submit for Review',
    approve: 'Approve Task',
    reject: 'Reject Task',
    publish: 'Publish Task',
    draft: 'Resubmit as Draft',
  };

  return h('div', {
    style: 'position: fixed; top: 0; left: 0; right: 0; bottom: 0; background: rgba(0,0,0,0.7); display: flex; align-items: center; justify-content: center; z-index: 1000;',
    onClick: (e: MouseEvent) => {
      if (e.target === e.currentTarget) {
        state.showTransitionForm = null;
        app.reload();
      }
    }
  }, [
    h('div', {
      style: 'background: #16213e; padding: 2rem; border-radius: 8px; max-width: 500px; width: 90%; box-shadow: 0 8px 32px rgba(0,0,0,0.5);'
    }, [
      h('h3', { style: 'margin: 0 0 1.5rem 0; color: #ff6b35;' }, actionLabels[action] || action),

      h('form', {
        onSubmit: (e: Event) => {
          e.preventDefault();
          const form = e.target as HTMLFormElement;
          const formData = new FormData(form);
          const data = Object.fromEntries(formData.entries());

          // Optimistic update
          const optimisticState: TaskState = {
            status: action === 'review' ? 'in_review' : action === 'approve' ? 'approved' : action === 'reject' ? 'rejected' : action === 'publish' ? 'published' : 'draft',
            label: action === 'review' ? 'In Review' : action === 'approve' ? 'Approved' : action === 'reject' ? 'Rejected' : action === 'publish' ? 'Published' : 'Draft',
            reviewer: data.reviewer as string,
            ...(data.reason && { reason: data.reason as string }),
            ...(data.url && { url: data.url as string }),
          };

          state.optimisticTransition = { taskId, newState: optimisticState };
          state.showTransitionForm = null;
          state.isTransitioning = true;
          app.reload();

          // Use Switchback's POST capability!
          app.visit(`/task/${taskId}`, {
            method: 'post',
            data: { action, ...data },
            preserveScroll: true,
            onStart: () => {
              console.log('🚀 Starting transition...');
            },
            onSuccess: (page) => {
              state.optimisticTransition = null;
              state.isTransitioning = false;
              showToast(`Task ${action} successful!`, 'success');
            },
            onError: (error) => {
              state.optimisticTransition = null;
              state.isTransitioning = false;
              showToast(error.error || 'Transition failed', 'error');
              app.reload();
            },
            onFinish: () => {
              console.log('✅ Transition complete');
            }
          });
        }
      }, [
        // Reviewer field (all actions need this)
        h('div', { style: 'margin-bottom: 1rem;' }, [
          h('label', { style: 'display: block; margin-bottom: 0.5rem; color: #eee;' }, 'Reviewer Name *'),
          h('input', {
            type: 'text',
            name: 'reviewer',
            required: true,
            placeholder: 'Enter reviewer name',
            style: 'width: 100%; padding: 0.75rem; border: 1px solid #333; border-radius: 4px; background: #1a1a2e; color: #eee; font-size: 1rem;'
          }),
        ]),

        // Reason field (only for reject)
        action === 'reject' && h('div', { style: 'margin-bottom: 1rem;' }, [
          h('label', { style: 'display: block; margin-bottom: 0.5rem; color: #eee;' }, 'Rejection Reason *'),
          h('textarea', {
            name: 'reason',
            required: true,
            placeholder: 'Explain why this task is being rejected',
            rows: 3,
            style: 'width: 100%; padding: 0.75rem; border: 1px solid #333; border-radius: 4px; background: #1a1a2e; color: #eee; font-size: 1rem; resize: vertical;'
          }),
        ]),

        // URL field (only for publish) - auto-generated with option to edit
        action === 'publish' && (() => {
          const currentTask = state.tasks.find(t => t.id === taskId);
          const slug = currentTask?.title.toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-|-$/g, '') || 'task';
          const autoUrl = `/published/${slug}`;

          return h('div', { style: 'margin-bottom: 1rem;' }, [
            h('label', { style: 'display: block; margin-bottom: 0.5rem; color: #eee;' }, 'Publish URL (auto-generated)'),
            h('input', {
              type: 'text',
              name: 'url',
              value: autoUrl,
              placeholder: '/published/...',
              style: 'width: 100%; padding: 0.75rem; border: 1px solid #333; border-radius: 4px; background: #1a1a2e; color: #eee; font-size: 1rem;'
            }),
            h('p', { style: 'margin: 0.5rem 0 0 0; color: #999; font-size: 0.85rem;' }, 'Auto-generated from task title. You can edit if needed.'),
          ]);
        })(),

        // Action buttons
        h('div', { style: 'display: flex; gap: 0.75rem; margin-top: 1.5rem;' }, [
          h('button', {
            type: 'submit',
            style: `flex: 1; padding: 0.75rem 1.5rem; border: none; border-radius: 4px; font-size: 1rem; cursor: pointer; background: ${
              action === 'approve' ? '#4caf50' :
              action === 'publish' ? '#9c27b0' :
              action === 'reject' ? '#f44336' :
              '#2196f3'
            }; color: white; font-weight: bold;`
          }, 'Confirm'),
          h('button', {
            type: 'button',
            onClick: () => {
              state.showTransitionForm = null;
              app.reload();
            },
            style: 'flex: 1; padding: 0.75rem 1.5rem; border: 1px solid #333; border-radius: 4px; font-size: 1rem; cursor: pointer; background: transparent; color: #eee;'
          }, 'Cancel'),
        ]),
      ]),
    ]),
  ]);
};

// ============================================================================
// PAGES - Switchback Components
// ============================================================================

const PublishedTaskPage = (props: { task: Task }) => {
  const { task } = props;

  return Layout([
    h('div', {}, [
      h('button', {
        style: 'background: #333; color: white; border: none; padding: 0.5rem 1rem; border-radius: 4px; cursor: pointer; margin-bottom: 1.5rem;',
        onClick: () => app.visit('/', { preserveScroll: true }),
      }, '← Back to Tasks'),

      h('div', {
        style: 'background: linear-gradient(135deg, #9c27b0 0%, #673ab7 100%); padding: 3rem 2rem; border-radius: 12px; margin-bottom: 2rem; box-shadow: 0 8px 24px rgba(156,39,176,0.3);'
      }, [
        h('div', { style: 'max-width: 800px; margin: 0 auto;' }, [
          h('div', { style: 'display: inline-block; background: rgba(255,255,255,0.2); padding: 0.5rem 1rem; border-radius: 20px; margin-bottom: 1rem;' }, [
            h('span', { style: 'color: white; font-size: 0.9rem; font-weight: bold;' }, '📄 PUBLISHED')
          ]),
          h('h1', { style: 'margin: 0 0 1rem 0; color: white; font-size: 2.5rem;' }, task.title),
          h('p', { style: 'color: rgba(255,255,255,0.9); font-size: 1.1rem; line-height: 1.6; margin: 0;' }, task.description),
        ]),
      ]),

      h('div', {
        style: 'background: #16213e; padding: 2rem; border-radius: 8px; margin-bottom: 2rem;'
      }, [
        h('h3', { style: 'color: #ff6b35; margin: 0 0 1rem 0;' }, 'Publication Details'),

        task.state.status === 'published' && h('div', { style: 'display: grid; gap: 1rem;' }, [
          h('div', {}, [
            h('strong', { style: 'color: #999; display: block; margin-bottom: 0.25rem;' }, 'Published By'),
            h('span', { style: 'color: #eee;' }, task.state.reviewer || 'Unknown'),
          ]),
          h('div', {}, [
            h('strong', { style: 'color: #999; display: block; margin-bottom: 0.25rem;' }, 'Published At'),
            h('span', { style: 'color: #eee;' },
              task.state.published_at
                ? new Date(task.state.published_at * 1000).toLocaleString()
                : 'Unknown'),
          ]),
          h('div', {}, [
            h('strong', { style: 'color: #999; display: block; margin-bottom: 0.25rem;' }, 'Priority'),
            PriorityBadge(task.priority),
          ]),
        ]),
      ]),

      h('div', {
        style: 'background: rgba(255,107,53,0.1); padding: 1.5rem; border-radius: 8px; border-left: 4px solid #ff6b35;'
      }, [
        h('p', { style: 'margin: 0; color: #eee;' }, [
          '✨ This is a published task! You can share this URL with others. Click "View Details" to see the full task information.'
        ]),
        h('button', {
          style: 'margin-top: 1rem; background: #ff6b35; color: white; border: none; padding: 0.75rem 1.5rem; border-radius: 4px; cursor: pointer; font-size: 1rem;',
          onClick: () => app.visit(`/task/${task.id}`),
        }, 'View Task Details →'),
      ]),
    ]),
  ]);
};

const MainPage = (props: TaskListProps) => {
  if (state.tasks.length === 0) {
    // Fetch tasks on first load
    fetchTasks().then(tasks => {
      state.tasks = tasks;
      app.reload();
    });
    return Layout([h('div', {}, 'Loading tasks...')]);
  }

  return Layout([
    h('div', {}, [
      h('h2', { style: 'color: #ff6b35; margin-bottom: 1rem;' }, '📋 Workflow Tasks'),

      h('div', {
        style: 'background: #16213e; padding: 1.5rem; border-radius: 8px; border-left: 4px solid #ff6b35; margin-bottom: 2rem;'
      }, [
        h('p', { style: 'margin: 0 0 0.5rem 0; line-height: 1.6;' },
          'This demo showcases OCaml\'s type-safe state machine: pattern matching prevents invalid transitions, algebraic data types model workflows, and Switchback provides instant navigation.'),
        h('p', { style: 'margin: 0; color: #999; font-size: 0.9rem;' },
          'Click any task to view details and trigger state transitions.'),
      ]),

      h('div', {
        style: 'display: grid; gap: 1rem;'
      }, state.tasks.map(task => {
        // Apply optimistic state if exists
        const displayState = state.optimisticTransition && state.optimisticTransition.taskId === task.id
          ? state.optimisticTransition.newState
          : task.state;

        return h('div', {
          key: task.id.toString(),
          style: `background: #16213e; padding: 1.5rem; border-radius: 8px; border-left: 4px solid ${
            displayState.status === 'published' ? '#9c27b0' :
            displayState.status === 'approved' ? '#4caf50' :
            displayState.status === 'in_review' ? '#2196f3' :
            displayState.status === 'rejected' ? '#f44336' : '#607d8b'
          }; cursor: pointer; transition: all 0.2s; ${state.optimisticTransition?.taskId === task.id ? 'opacity: 0.7; border-style: dashed;' : ''}`,
          onClick: () => {
            app.visit(`/task/${task.id}`);  // Switchback navigation!
          },
          onMouseOver: (e: MouseEvent) => {
            (e.target as HTMLElement).style.background = '#1e2a47';
          },
          onMouseOut: (e: MouseEvent) => {
            (e.target as HTMLElement).style.background = '#16213e';
          },
        }, [
          h('div', { style: 'display: flex; justify-content: space-between; align-items: start; margin-bottom: 0.75rem;' }, [
            h('h3', { style: 'margin: 0; color: white;' }, task.title),
            h('div', { style: 'display: flex; gap: 0.5rem;' }, [
              PriorityBadge(task.priority),
              StateBadge(displayState),
            ]),
          ]),
          h('p', { style: 'color: #bbb; margin: 0;' }, task.description),
          displayState.reviewer && h('p', { style: 'color: #999; margin-top: 0.5rem; font-size: 0.9rem;' }, `Reviewer: ${displayState.reviewer}`),
          displayState.url && h('p', { style: 'color: #9c27b0; margin-top: 0.5rem; font-size: 0.9rem;' }, [
            '📄 Published at: ',
            h('a', {
              href: displayState.url,
              onClick: (e: MouseEvent) => {
                e.preventDefault();
                e.stopPropagation(); // Don't trigger task detail navigation
                if (displayState.url?.startsWith('/')) {
                  app.visit(displayState.url);
                } else {
                  window.open(displayState.url, '_blank');
                }
              },
              style: 'color: #9c27b0; text-decoration: underline; cursor: pointer;'
            }, displayState.url),
          ]),
        ]);
      })),
    ]),
  ]);
};

const TaskDetailPage = (props: TaskDetailProps) => {
  const { task, availableActions } = props;
  state.currentTask = task;

  const handleTransition = (action: string) => {
    // Show inline form instead of prompt!
    state.showTransitionForm = { taskId: task.id, action };
    app.reload();
  };

  // Apply optimistic state if exists
  const displayState = state.optimisticTransition && state.optimisticTransition.taskId === task.id
    ? state.optimisticTransition.newState
    : task.state;

  return Layout([
    h('div', {}, [
      h('button', {
        style: 'background: #333; color: white; border: none; padding: 0.5rem 1rem; border-radius: 4px; cursor: pointer; margin-bottom: 1.5rem;',
        onClick: () => app.visit('/', { preserveScroll: true }),  // Switchback navigation with scroll preservation!
      }, '← Back to Tasks'),

      h('div', {
        style: `background: #16213e; padding: 2rem; border-radius: 8px; border-left: 4px solid ${
          displayState.status === 'published' ? '#9c27b0' :
          displayState.status === 'approved' ? '#4caf50' :
          displayState.status === 'in_review' ? '#2196f3' :
          displayState.status === 'rejected' ? '#f44336' : '#607d8b'
        }; ${state.isTransitioning ? 'opacity: 0.7;' : ''}`
      }, [
        h('div', { style: 'display: flex; justify-content: space-between; align-items: start; margin-bottom: 1.5rem;' }, [
          h('h2', { style: 'margin: 0; color: white;' }, task.title),
          h('div', { style: 'display: flex; gap: 0.5rem;' }, [
            PriorityBadge(task.priority),
            StateBadge(displayState),
          ]),
        ]),

        h('p', { style: 'color: #bbb; line-height: 1.6; margin-bottom: 1.5rem;' }, task.description),

        displayState.reviewer && h('div', {
          style: 'background: rgba(255,255,255,0.05); padding: 1rem; border-radius: 4px; margin-bottom: 1.5rem;'
        }, [
          h('strong', { style: 'color: #ff6b35;' }, 'Reviewer: '),
          h('span', {}, displayState.reviewer),
        ]),

        displayState.reason && h('div', {
          style: 'background: rgba(244,67,54,0.1); padding: 1rem; border-radius: 4px; margin-bottom: 1.5rem; border-left: 3px solid #f44336;'
        }, [
          h('strong', { style: 'color: #f44336;' }, 'Rejection Reason: '),
          h('p', { style: 'margin: 0.5rem 0 0 0;' }, displayState.reason),
        ]),

        displayState.url && h('div', {
          style: 'background: rgba(156,39,176,0.1); padding: 1rem; border-radius: 4px; margin-bottom: 1.5rem; border-left: 3px solid #9c27b0;'
        }, [
          h('strong', { style: 'color: #9c27b0;' }, 'Published URL: '),
          h('a', {
            href: displayState.url,
            onClick: (e: MouseEvent) => {
              e.preventDefault();
              // Use Switchback navigation for internal URLs
              if (displayState.url?.startsWith('/')) {
                app.visit(displayState.url);
              } else {
                // External URL - open in new tab
                window.open(displayState.url, '_blank');
              }
            },
            style: 'color: #9c27b0; text-decoration: underline; cursor: pointer;'
          }, displayState.url),
          h('span', { style: 'margin-left: 0.5rem; color: #999; font-size: 0.85rem;' }, '(click to navigate)'),
        ]),

        // Available actions
        availableActions.length > 0 && h('div', { style: 'margin-top: 2rem;' }, [
          h('h3', { style: 'color: #ff6b35; margin-bottom: 1rem;' }, '🔄 Available Transitions'),
          h('div', { style: 'display: flex; gap: 0.75rem; flex-wrap: wrap;' }, availableActions.map(action =>
            h('button', {
              key: action,
              style: `background: ${
                action === 'approve' ? '#4caf50' :
                action === 'publish' ? '#9c27b0' :
                action === 'reject' ? '#f44336' :
                '#2196f3'
              }; color: white; border: none; padding: 0.75rem 1.5rem; border-radius: 4px; cursor: pointer; font-size: 1rem; text-transform: capitalize;`,
              onClick: () => handleTransition(action),
            }, action)
          )),
        ]),
      ]),
    ]),
  ]);
};



// ============================================================================
// SWITCHBACK APP
// ============================================================================

const pages = {
  'Main': MainPage,
  'TaskDetail': TaskDetailPage,
  'PublishedTask': PublishedTaskPage,
};

// Extend window to include initialPage
declare global {
  interface Window {
    initialPage: any;
  }
}

const app = newSwitchback({
  resolve: (name: string) => {
    const component = pages[name as keyof typeof pages];
    if (!component) {
      throw new Error(`Component "${name}" not found`);
    }
    return component;
  },

  setup: ({ el, App, props }: { el: Element; App: any; props: any }) => {
    el.innerHTML = '';
    el.appendChild(App(props));
  },

  initialPage: window.initialPage,

  progress: {
    delay: 250,
    color: '#ff6b35',
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('🐫 OCaml + Switchback initialized!');
