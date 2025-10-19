/**
 * OCaml + Switchback Demo
 * Showcases Switchback's instant navigation with OCaml's type-safe state machine
 *
 * Switchback Features Demonstrated:
 * - app.visit() for instant SPA navigation
 * - Optimistic updates for state transitions
 * - Progress indicators during async operations
 * - Form handling with POST requests
 * - Conditional rendering based on state
 * - Error recovery when transitions fail
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

interface HomeProps {
  stats: {
    draft: number;
    inReview: number;
    approved: number;
    published: number;
    total: number;
  };
}

interface TaskListProps {
  tasks: Task[];
}

interface TaskDetailProps {
  task: Task;
  availableActions: string[];
}

interface AboutProps {
  version: string;
  backend: string;
  features: string[];
}

interface ErrorProps {
  message: string;
}

// ============================================================================
// GLOBAL STATE
// ============================================================================

const state = {
  tasks: [] as Task[],
  currentTask: null as Task | null,
  optimisticTransition: null as { taskId: number; newState: TaskState } | null,
  isTransitioning: false,
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

// Layout wrapper with navigation
const Layout = (children: any[]) => {
  return h('div', { style: 'min-height: 100vh; background: #1a1a2e; color: #eee;' }, [
    // Header
    h('header', {
      style: 'background: linear-gradient(135deg, #ff6b35 0%, #f7931e 100%); padding: 1.5rem; box-shadow: 0 4px 6px rgba(0,0,0,0.3);'
    }, [
      h('div', { style: 'max-width: 1200px; margin: 0 auto;' }, [
        h('h1', { style: 'margin: 0 0 1rem 0; font-size: 2rem; color: white;' }, '🐫 OCaml Workflow'),
        h('nav', { style: 'display: flex; gap: 1rem; flex-wrap: wrap;' }, [
          h('a', {
            href: '/',
            'data-swbk': '',
            style: 'color: white; text-decoration: none; padding: 0.5rem 1rem; background: rgba(255,255,255,0.2); border-radius: 4px; transition: all 0.2s;',
            onMouseOver: (e: MouseEvent) => {
              (e.target as HTMLElement).style.background = 'rgba(255,255,255,0.3)';
            },
            onMouseOut: (e: MouseEvent) => {
              (e.target as HTMLElement).style.background = 'rgba(255,255,255,0.2)';
            },
          }, 'Home'),
          h('a', {
            href: '/tasks',
            'data-swbk': '',
            style: 'color: white; text-decoration: none; padding: 0.5rem 1rem; background: rgba(255,255,255,0.2); border-radius: 4px; transition: all 0.2s;',
            onMouseOver: (e: MouseEvent) => {
              (e.target as HTMLElement).style.background = 'rgba(255,255,255,0.3)';
            },
            onMouseOut: (e: MouseEvent) => {
              (e.target as HTMLElement).style.background = 'rgba(255,255,255,0.2)';
            },
          }, 'Tasks'),
          h('a', {
            href: '/about',
            'data-swbk': '',
            style: 'color: white; text-decoration: none; padding: 0.5rem 1rem; background: rgba(255,255,255,0.2); border-radius: 4px; transition: all 0.2s;',
            onMouseOver: (e: MouseEvent) => {
              (e.target as HTMLElement).style.background = 'rgba(255,255,255,0.3)';
            },
            onMouseOut: (e: MouseEvent) => {
              (e.target as HTMLElement).style.background = 'rgba(255,255,255,0.2)';
            },
          }, 'About'),
        ]),
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

// ============================================================================
// PAGES - Switchback Components
// ============================================================================

const HomePage = (props: HomeProps) => {
  const { stats } = props;

  return Layout([
    h('div', {}, [
      h('h2', { style: 'color: #ff6b35; margin-bottom: 1.5rem;' }, '📊 Workflow Dashboard'),

      h('div', {
        style: 'display: grid; grid-template-columns: repeat(auto-fit, minmax(200px, 1fr)); gap: 1.5rem; margin-bottom: 2rem;'
      }, [
        h('div', {
          style: 'background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 1.5rem; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.3);'
        }, [
          h('div', { style: 'font-size: 2.5rem; font-weight: bold; margin-bottom: 0.5rem;' }, stats.draft.toString()),
          h('div', { style: 'color: rgba(255,255,255,0.9);' }, 'Draft'),
        ]),
        h('div', {
          style: 'background: linear-gradient(135deg, #2196f3 0%, #1976d2 100%); padding: 1.5rem; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.3);'
        }, [
          h('div', { style: 'font-size: 2.5rem; font-weight: bold; margin-bottom: 0.5rem;' }, stats.inReview.toString()),
          h('div', { style: 'color: rgba(255,255,255,0.9);' }, 'In Review'),
        ]),
        h('div', {
          style: 'background: linear-gradient(135deg, #4caf50 0%, #388e3c 100%); padding: 1.5rem; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.3);'
        }, [
          h('div', { style: 'font-size: 2.5rem; font-weight: bold; margin-bottom: 0.5rem;' }, stats.approved.toString()),
          h('div', { style: 'color: rgba(255,255,255,0.9);' }, 'Approved'),
        ]),
        h('div', {
          style: 'background: linear-gradient(135deg, #9c27b0 0%, #7b1fa2 100%); padding: 1.5rem; border-radius: 8px; box-shadow: 0 4px 6px rgba(0,0,0,0.3);'
        }, [
          h('div', { style: 'font-size: 2.5rem; font-weight: bold; margin-bottom: 0.5rem;' }, stats.published.toString()),
          h('div', { style: 'color: rgba(255,255,255,0.9);' }, 'Published'),
        ]),
      ]),

      h('div', {
        style: 'background: #16213e; padding: 2rem; border-radius: 8px; border-left: 4px solid #ff6b35;'
      }, [
        h('h3', { style: 'color: #ff6b35; margin-bottom: 1rem;' }, '💡 About This Demo'),
        h('p', { style: 'line-height: 1.6; margin-bottom: 1rem;' }, 'This demo showcases OCaml\'s type-safe state machine with Switchback\'s instant navigation:'),
        h('ul', { style: 'list-style: none; padding-left: 0;' }, [
          h('li', { style: 'margin-bottom: 0.5rem; padding-left: 1.5rem; position: relative;' }, [
            h('span', { style: 'position: absolute; left: 0;' }, '✓'),
            ' Pattern matching prevents invalid state transitions'
          ]),
          h('li', { style: 'margin-bottom: 0.5rem; padding-left: 1.5rem; position: relative;' }, [
            h('span', { style: 'position: absolute; left: 0;' }, '✓'),
            ' Algebraic data types model complex workflows'
          ]),
          h('li', { style: 'margin-bottom: 0.5rem; padding-left: 1.5rem; position: relative;' }, [
            h('span', { style: 'position: absolute; left: 0;' }, '✓'),
            ' Switchback provides instant navigation with app.visit()'
          ]),
          h('li', { style: 'margin-bottom: 0.5rem; padding-left: 1.5rem; position: relative;' }, [
            h('span', { style: 'position: absolute; left: 0;' }, '✓'),
            ' Optimistic updates for smooth UX'
          ]),
        ]),
        h('button', {
          style: 'background: #ff6b35; color: white; border: none; padding: 0.75rem 1.5rem; border-radius: 4px; font-size: 1rem; cursor: pointer; margin-top: 1rem;',
          onClick: (e: Event) => {
            e.preventDefault();
            app.visit('/tasks');  // Switchback instant navigation!
          },
        }, 'View Tasks →'),
      ]),
    ]),
  ]);
};

const TaskListPage = (props: TaskListProps) => {
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
      h('h2', { style: 'color: #ff6b35; margin-bottom: 1.5rem;' }, '📋 Task List'),

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
        ]);
      })),
    ]),
  ]);
};

const TaskDetailPage = (props: TaskDetailProps) => {
  const { task, availableActions } = props;
  state.currentTask = task;

  const handleTransition = (action: string) => {
    const reviewer = prompt(`Enter reviewer name for ${action}:`) || 'System';
    let reason: string | undefined;
    let url: string | undefined;

    if (action === 'reject') {
      reason = prompt('Enter rejection reason:') || 'No reason provided';
    } else if (action === 'publish') {
      url = prompt('Enter publish URL:') || '/published';
    }

    // OPTIMISTIC UPDATE - Update UI immediately!
    const optimisticState: TaskState = {
      status: action === 'review' ? 'in_review' : action === 'approve' ? 'approved' : action === 'reject' ? 'rejected' : action === 'publish' ? 'published' : 'draft',
      label: action === 'review' ? 'In Review' : action === 'approve' ? 'Approved' : action === 'reject' ? 'Rejected' : action === 'publish' ? 'Published' : 'Draft',
      reviewer,
      ...(reason && { reason }),
      ...(url && { url }),
    };

    state.optimisticTransition = { taskId: task.id, newState: optimisticState };
    app.reload();

    // Send to server
    transitionTask(task.id, action, reviewer, reason, url).then(result => {
      state.optimisticTransition = null;

      if (result.success && result.task) {
        // Update succeeded - navigate to updated task
        app.visit(`/task/${task.id}`);
      } else {
        // Update failed - show error and revert
        alert(`Transition failed: ${result.error}`);
        app.reload();
      }
    });
  };

  // Apply optimistic state if exists
  const displayState = state.optimisticTransition && state.optimisticTransition.taskId === task.id
    ? state.optimisticTransition.newState
    : task.state;

  return Layout([
    h('div', {}, [
      h('button', {
        style: 'background: #333; color: white; border: none; padding: 0.5rem 1rem; border-radius: 4px; cursor: pointer; margin-bottom: 1.5rem;',
        onClick: () => app.visit('/tasks'),  // Switchback navigation!
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
            style: 'color: #9c27b0; text-decoration: underline;'
          }, displayState.url),
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

const AboutPage = (props: AboutProps) => {
  return Layout([
    h('div', {}, [
      h('h2', { style: 'color: #ff6b35; margin-bottom: 1.5rem;' }, 'ℹ️ About This Demo'),

      h('div', {
        style: 'background: #16213e; padding: 2rem; border-radius: 8px; margin-bottom: 1.5rem;'
      }, [
        h('h3', { style: 'color: #ff6b35; margin-bottom: 1rem;' }, 'Backend'),
        h('p', {}, `${props.backend} - Pure standard library, no frameworks!`),
      ]),

      h('div', {
        style: 'background: #16213e; padding: 2rem; border-radius: 8px; margin-bottom: 1.5rem;'
      }, [
        h('h3', { style: 'color: #ff6b35; margin-bottom: 1rem;' }, 'Features'),
        h('ul', { style: 'list-style-position: inside;' }, props.features.map(feature =>
          h('li', { key: feature, style: 'margin-bottom: 0.5rem;' }, feature)
        )),
      ]),

      h('div', {
        style: 'background: #16213e; padding: 2rem; border-radius: 8px;'
      }, [
        h('h3', { style: 'color: #ff6b35; margin-bottom: 1rem;' }, 'Switchback Features Used'),
        h('ul', { style: 'list-style-position: inside;' }, [
          h('li', { style: 'margin-bottom: 0.5rem;' }, 'app.visit() for instant navigation'),
          h('li', { style: 'margin-bottom: 0.5rem;' }, 'Optimistic updates for smooth UX'),
          h('li', { style: 'margin-bottom: 0.5rem;' }, 'Progress indicators during async operations'),
          h('li', { style: 'margin-bottom: 0.5rem;' }, 'Form handling with POST requests'),
          h('li', { style: 'margin-bottom: 0.5rem;' }, 'Conditional rendering based on task state'),
        ]),
      ]),
    ]),
  ]);
};

const ErrorPage = (props: ErrorProps) => {
  return Layout([
    h('div', {
      style: 'background: #16213e; padding: 2rem; border-radius: 8px; border-left: 4px solid #f44336; text-align: center;'
    }, [
      h('h2', { style: 'color: #f44336; margin-bottom: 1rem;' }, '⚠️ Error'),
      h('p', { style: 'color: #bbb; margin-bottom: 1.5rem;' }, props.message),
      h('button', {
        style: 'background: #ff6b35; color: white; border: none; padding: 0.75rem 1.5rem; border-radius: 4px; cursor: pointer;',
        onClick: () => app.visit('/'),
      }, 'Go Home'),
    ]),
  ]);
};

// ============================================================================
// SWITCHBACK APP
// ============================================================================

const pages = {
  'Home': HomePage,
  'TaskList': TaskListPage,
  'TaskDetail': TaskDetailPage,
  'About': AboutPage,
  'Error': ErrorPage,
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
