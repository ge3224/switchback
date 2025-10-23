/**
 * Client-side app for Go recipe
 * Demonstrates TRUE CONCURRENCY with real-time worker pool visualization
 * Watch multiple goroutines process jobs in PARALLEL across CPU cores!
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

// Global state for real-time updates
const state = {
  workers: [],
  jobs: [],
  autoRefresh: null,
  pendingQueue: [], // Local queue before submission
};

// Inject cyberpunk blue theme
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: 'Courier New', 'Consolas', monospace;
    background: #0f0f23;
    color: #16f4d0;
    line-height: 1.6;
  }

  main {
    max-width: 1200px;
    margin: 2rem auto;
    padding: 0 2rem;
  }

  h1 {
    font-size: 2.5rem;
    margin-bottom: 1rem;
    color: #16f4d0;
    text-shadow: 0 0 20px rgba(22, 244, 208, 0.6);
  }

  h2 {
    font-size: 1.8rem;
    color: #16f4d0;
    margin: 2rem 0 1rem;
    text-shadow: 0 0 15px rgba(22, 244, 208, 0.5);
  }

  .badge {
    background: #16f4d0;
    color: #0f0f23;
    padding: 0.3rem 0.8rem;
    border-radius: 6px;
    font-size: 1rem;
    margin-left: 1rem;
    display: inline-block;
    font-weight: bold;
  }

  .terminal-box {
    background: #0f0f23;
    border: 2px solid #16f4d0;
    border-radius: 8px;
    padding: 1.5rem;
    margin: 1.5rem 0;
    font-family: 'Courier New', monospace;
    box-shadow: 0 0 15px rgba(22, 244, 208, 0.2);
  }

  .stats {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
    gap: 1.5rem;
    margin: 2rem 0;
  }

  .stat-card {
    background: #1a1a2e;
    border: 2px solid #16f4d0;
    border-radius: 8px;
    padding: 1.5rem;
    text-align: center;
    box-shadow: 0 0 15px rgba(22, 244, 208, 0.2);
  }

  .stat-card strong {
    font-size: 2.5rem;
    color: #16f4d0;
    display: block;
    margin-bottom: 0.5rem;
    text-shadow: 0 0 15px rgba(22, 244, 208, 0.6);
  }

  button, .btn {
    background: #16f4d0;
    color: #0f0f23;
    border: none;
    padding: 0.75rem 1.5rem;
    border-radius: 6px;
    cursor: pointer;
    font-weight: bold;
    font-family: 'Courier New', monospace;
    text-decoration: none;
    display: inline-block;
    transition: all 0.2s;
    box-shadow: 0 0 10px rgba(22, 244, 208, 0.4);
  }

  button:hover, .btn:hover {
    transform: translateY(-2px);
    box-shadow: 0 4px 20px rgba(22, 244, 208, 0.6);
  }

  button:disabled {
    opacity: 0.5;
    cursor: not-allowed;
    transform: none;
  }

  input {
    flex: 1;
    background: #1a1a2e;
    border: 2px solid #16f4d0;
    color: #16f4d0;
    padding: 0.75rem;
    border-radius: 6px;
    font-family: 'Courier New', monospace;
    font-size: 1rem;
    transition: all 0.2s;
  }

  input:focus {
    outline: none;
    border-color: #00d9ff;
    box-shadow: 0 0 15px rgba(22, 244, 208, 0.3);
  }

  input::placeholder {
    color: rgba(22, 244, 208, 0.4);
  }

  .worker-grid {
    display: grid;
    grid-template-columns: repeat(auto-fit, minmax(250px, 1fr));
    gap: 1rem;
    margin: 2rem 0;
  }

  .worker-card {
    background: #1a1a2e;
    border: 2px solid #16f4d0;
    border-radius: 8px;
    padding: 1.5rem;
    transition: all 0.3s;
  }

  .worker-card.working {
    border-color: #ffd700;
    box-shadow: 0 0 20px rgba(255, 215, 0, 0.4);
    animation: working 1s infinite;
  }

  @keyframes working {
    0%, 100% { box-shadow: 0 0 20px rgba(255, 215, 0, 0.4); }
    50% { box-shadow: 0 0 30px rgba(255, 215, 0, 0.6); }
  }

  .worker-status {
    display: inline-block;
    padding: 0.3rem 0.8rem;
    border-radius: 4px;
    font-size: 0.9rem;
    font-weight: bold;
  }

  .worker-status.idle {
    background: #16f4d0;
    color: #0f0f23;
  }

  .worker-status.working {
    background: #ffd700;
    color: #0f0f23;
  }

  .job-list {
    margin: 2rem 0;
  }

  .job-item {
    background: #1a1a2e;
    border: 2px solid #16f4d0;
    border-radius: 8px;
    padding: 1rem;
    margin-bottom: 1rem;
    display: flex;
    justify-content: space-between;
    align-items: center;
    transition: all 0.3s;
  }

  .job-item.pending {
    border-color: #16f4d0;
    opacity: 0.7;
  }

  .job-item.processing {
    border-color: #ffd700;
    box-shadow: 0 0 15px rgba(255, 215, 0, 0.4);
  }

  .job-item.completed {
    border-color: #00ff88;
    box-shadow: 0 0 10px rgba(0, 255, 136, 0.3);
  }

  .job-badge {
    padding: 0.3rem 0.8rem;
    border-radius: 4px;
    font-size: 0.85rem;
    font-weight: bold;
    margin-left: 0.5rem;
  }

  .job-badge.pending {
    background: #16f4d0;
    color: #0f0f23;
  }

  .job-badge.processing {
    background: #ffd700;
    color: #0f0f23;
  }

  .job-badge.completed {
    background: #00ff88;
    color: #0f0f23;
  }

  .form-row {
    display: flex;
    gap: 1rem;
    margin: 1.5rem 0;
  }

  .info-box {
    background: #1a1a2e;
    border: 2px solid #16f4d0;
    border-radius: 8px;
    padding: 1.5rem;
    margin: 1.5rem 0;
    box-shadow: 0 0 15px rgba(22, 244, 208, 0.2);
  }

  .factors {
    font-size: 0.9rem;
    color: #00d9ff;
    margin-top: 0.5rem;
    word-break: break-all;
  }

  ul {
    margin-left: 2rem;
    margin-top: 1rem;
  }

  li {
    margin: 0.5rem 0;
    color: #00d9ff;
  }

  code {
    background: #1a1a2e;
    padding: 0.2rem 0.5rem;
    border-radius: 3px;
    color: #ffd700;
  }

  .quick-numbers {
    display: flex;
    gap: 0.5rem;
    margin: 1rem 0;
    flex-wrap: wrap;
  }

  .quick-btn {
    background: #1a1a2e;
    border: 1px solid #16f4d0;
    color: #16f4d0;
    padding: 0.5rem 1rem;
    font-size: 0.9rem;
  }

  .quick-btn:hover {
    background: rgba(22, 244, 208, 0.1);
  }

  .pending-queue {
    background: #1a1a2e;
    border: 2px solid #00d9ff;
    border-radius: 8px;
    padding: 1.5rem;
    margin: 1.5rem 0;
    box-shadow: 0 0 15px rgba(0, 217, 255, 0.2);
  }

  .pending-queue h3 {
    margin: 0 0 1rem 0;
    color: #00d9ff;
  }

  .queue-items {
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
    margin-bottom: 1rem;
  }

  .queue-item {
    background: #0f0f23;
    border: 1px solid #00d9ff;
    color: #00d9ff;
    padding: 0.5rem 1rem;
    border-radius: 4px;
    display: flex;
    align-items: center;
    gap: 0.5rem;
  }

  .queue-item button {
    background: transparent;
    border: none;
    color: #ff4444;
    cursor: pointer;
    padding: 0;
    font-size: 1rem;
  }

  .btn-execute {
    background: #ffd700;
    color: #0f0f23;
    font-size: 1.1rem;
    padding: 1rem 2rem;
  }

  .btn-execute:disabled {
    background: #666;
    color: #999;
  }

  .section-header {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin: 2rem 0 1rem;
  }

  .section-header h2 {
    margin: 0;
  }

  .btn-clear-all {
    background: #ff4444;
    color: #fff;
    font-size: 0.9rem;
    padding: 0.5rem 1rem;
  }

  .btn-clear-all:disabled {
    background: #666;
    color: #999;
  }
`;
document.head.appendChild(style);

// Layout component
function Layout(children) {
  const header = h('div', {
    style: {
      background: '#1a1a2e',
      borderBottom: '2px solid #16f4d0',
      padding: '1.5rem 2rem',
      boxShadow: '0 4px 12px rgba(22, 244, 208, 0.3)',
      textAlign: 'center'
    }
  },
    h('h1', { style: { margin: 0, fontSize: '2rem' } },
      '⚡ Go Concurrent Factorization',
      h('span', { class: 'badge', style: { marginLeft: '1rem' } }, 'TRUE CONCURRENCY')
    ),
    h('p', { style: { margin: '0.5rem 0 0 0', color: '#00d9ff', fontSize: '0.95rem' } },
      'Watch ', h('strong', {}, '4 goroutines'), ' process jobs in REAL parallel on multiple CPU cores'
    )
  );

  const main = h('main', {},
    children
  );

  const container = h('div', {});
  container.appendChild(header);
  container.appendChild(main);
  return container;
}

// Render worker cards
function renderWorkers() {
  const container = document.getElementById('worker-grid');
  if (!container) return;

  container.innerHTML = '';

  state.workers.forEach(worker => {
    const card = h('div', {
      class: `worker-card ${worker.status === 'working' ? 'working' : ''}`
    },
      h('h3', {}, `Worker ${worker.id}`),
      h('div', { style: { marginTop: '1rem' } },
        h('span', { class: `worker-status ${worker.status}` },
          worker.status === 'idle' ? '💤 Idle' : '⚡ Working'
        )
      ),
      worker.currentJob ? h('div', { style: { marginTop: '0.5rem', fontSize: '0.9rem', color: '#ffd700' } },
        `Processing: ${worker.currentJob}`
      ) : null
    );
    container.appendChild(card);
  });
}

// Render job list (with visual feedback optimization)
function renderJobs() {
  const container = document.getElementById('job-list');
  if (!container) return;

  // Update Clear All button state
  const clearAllBtn = document.getElementById('clear-all-btn');
  if (clearAllBtn) {
    const hasCompletedJobs = state.jobs.some(j => j.status === 'completed');
    clearAllBtn.disabled = !hasCompletedJobs;
  }

  // Check if we have any active jobs to show visual feedback
  const hasActiveJobs = state.jobs.some(j => j.status === 'pending' || j.status === 'processing');

  // Add a subtle indicator when actively processing
  if (hasActiveJobs) {
    container.style.opacity = '1';
  } else {
    container.style.opacity = '0.7';
  }

  container.innerHTML = '';

  if (state.jobs.length === 0) {
    const empty = h('div', { class: 'info-box', style: { textAlign: 'center' } },
      h('p', {}, '📝 No jobs yet. Submit a number above to start!')
    );
    container.appendChild(empty);
    return;
  }

  // Sort: pending/processing first, then completed
  const sortedJobs = [...state.jobs].sort((a, b) => {
    const order = { pending: 0, processing: 1, completed: 2 };
    return order[a.status] - order[b.status];
  });

  sortedJobs.slice().reverse().forEach(job => {
    const item = h('div', { class: `job-item ${job.status}`, 'data-job-id': job.id },
      h('div', { style: { flex: 1 } },
        h('div', {},
          h('strong', {}, job.number),
          h('span', { class: `job-badge ${job.status}` },
            job.status === 'pending' ? '⏳ Pending' :
            job.status === 'processing' ? `⚡ Worker ${job.workerId}` :
            `✓ ${job.duration.toFixed(2)}s`
          )
        ),
        job.factors && job.factors.length > 0 ? h('div', { class: 'factors' },
          `Factors: ${job.factors.join(' × ')}`
        ) : null
      ),
      job.status === 'completed' ? h('button', {
        class: 'quick-btn',
        onClick: () => clearJob(job.id)
      }, '🗑️') : null
    );
    container.appendChild(item);
  });
}

// Add number to pending queue
function addToQueue(number) {
  if (!state.pendingQueue.includes(number)) {
    state.pendingQueue.push(number);
    renderQueue();
  }
}

// Remove number from pending queue
function removeFromQueue(number) {
  const index = state.pendingQueue.indexOf(number);
  if (index > -1) {
    state.pendingQueue.splice(index, 1);
    renderQueue();
  }
}

// Execute all jobs in queue
async function executeQueue() {
  if (state.pendingQueue.length === 0) return;

  const numbersToSubmit = [...state.pendingQueue];
  state.pendingQueue = [];
  renderQueue();

  // Scroll to workers section smoothly at the very top of viewport
  const workersSection = document.getElementById('workers-section');
  if (workersSection) {
    const yOffset = -20; // Slight offset to ensure it's at the top
    const y = workersSection.getBoundingClientRect().top + window.pageYOffset + yOffset;
    window.scrollTo({ top: y, behavior: 'smooth' });
  }

  // Restart polling if it was stopped
  if (!state.autoRefresh) {
    console.log('Restarting auto-refresh');
    state.autoRefresh = setInterval(refreshStatus, 1000);
  }

  // Submit all jobs at once
  for (const number of numbersToSubmit) {
    try {
      await fetch('/api/jobs', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
          'X-Switchback': '1'
        },
        body: JSON.stringify({ number })
      });
    } catch (error) {
      console.error('Failed to submit job:', error);
    }
  }

  // Refresh status to show all jobs
  await refreshStatus();
}

// Render pending queue
function renderQueue() {
  const container = document.getElementById('pending-queue-items');
  if (!container) return;

  const executeBtn = document.getElementById('execute-btn');
  if (executeBtn) {
    executeBtn.disabled = state.pendingQueue.length === 0;
  }

  container.innerHTML = '';

  if (state.pendingQueue.length === 0) {
    const empty = h('div', { style: { color: '#666', fontSize: '0.9rem' } },
      'No numbers in queue. Add some above!'
    );
    container.appendChild(empty);
    return;
  }

  state.pendingQueue.forEach(number => {
    const item = h('div', { class: 'queue-item' },
      h('span', {}, number),
      h('button', {
        onClick: () => removeFromQueue(number),
        title: 'Remove from queue'
      }, '✕')
    );
    container.appendChild(item);
  });
}

// Generate random large numbers and add to queue
function addRandomNumbers() {
  // Generate 4 random 9-digit numbers
  for (let i = 0; i < 4; i++) {
    const min = 100000000;
    const max = 999999999;
    const randomNum = Math.floor(Math.random() * (max - min + 1)) + min;
    addToQueue(randomNum.toString());
  }
}

// Clear completed job
async function clearJob(jobId) {
  try {
    await fetch(`/api/jobs/${jobId}`, {
      method: 'DELETE',
      headers: { 'X-Switchback': '1' }
    });

    await refreshStatus();
  } catch (error) {
    console.error('Failed to clear job:', error);
  }
}

// Clear all completed jobs
async function clearAllCompleted() {
  const completedJobs = state.jobs.filter(j => j.status === 'completed');

  if (completedJobs.length === 0) return;

  try {
    // Clear all completed jobs in parallel
    await Promise.all(
      completedJobs.map(job =>
        fetch(`/api/jobs/${job.id}`, {
          method: 'DELETE',
          headers: { 'X-Switchback': '1' }
        })
      )
    );

    await refreshStatus();
  } catch (error) {
    console.error('Failed to clear completed jobs:', error);
  }
}

// Refresh status from server
async function refreshStatus() {
  try {
    const response = await fetch('/api/status', {
      headers: { 'X-Switchback': '1' }
    });
    const data = await response.json();

    state.workers = data.workers || [];
    state.jobs = data.jobs || [];

    renderWorkers();
    renderJobs();

    // Stop polling if all workers are idle and no active jobs
    const allIdle = state.workers.every(w => w.status === 'idle');
    const hasActiveJobs = state.jobs.some(j => j.status === 'pending' || j.status === 'processing');

    if (allIdle && !hasActiveJobs && state.autoRefresh) {
      console.log('All workers idle, stopping auto-refresh');
      clearInterval(state.autoRefresh);
      state.autoRefresh = null;
    }
  } catch (error) {
    console.error('Failed to refresh status:', error);
  }
}

// Page Components
const pages = {
  'Main': (props) => {
    // Initialize state
    state.workers = props.workers || [];
    state.jobs = props.jobs || [];

    // Start auto-refresh after a short delay to ensure page is ready
    if (state.autoRefresh) {
      clearInterval(state.autoRefresh);
    }
    setTimeout(() => {
      state.autoRefresh = setInterval(refreshStatus, 1000);
    }, 100);

    return Layout(
      h('div', {},
        // Stats overview
        h('div', { class: 'stats' },
          h('div', { class: 'stat-card' },
            h('strong', {}, props.stats.workers),
            h('div', {}, 'Worker Goroutines')
          ),
          h('div', { class: 'stat-card' },
            h('strong', {}, props.stats.activeJobs),
            h('div', {}, 'Active Jobs')
          ),
          h('div', { class: 'stat-card' },
            h('strong', {}, props.stats.completedJobs),
            h('div', {}, 'Completed')
          ),
          h('div', { class: 'stat-card' },
            h('strong', {}, props.stats.totalProcessed),
            h('div', {}, 'Total Processed')
          )
        ),

        // Input section
        h('div', { class: 'info-box' },
          h('h2', { style: { marginTop: 0 } }, '> Add Numbers to Queue'),
          h('form', {
            class: 'form-row',
            onSubmit: (e) => {
              e.preventDefault();
              const input = e.target.querySelector('input');
              const number = input.value.trim();
              if (number) {
                addToQueue(number);
                input.value = '';
              }
            }
          },
            h('input', {
              type: 'text',
              placeholder: 'Enter a large number (e.g., 123456789)',
              required: true,
              pattern: '[0-9]+',
              title: 'Please enter a positive integer'
            }),
            h('button', { type: 'submit' }, '➕ Add to Queue')
          ),
          h('div', { class: 'quick-numbers' },
            h('button', {
              class: 'btn',
              style: { background: '#00d9ff', fontSize: '0.95rem', padding: '0.6rem 1.2rem' },
              onClick: addRandomNumbers
            }, '🎲 Quick Demo (Add 4 Random Numbers)')
          )
        ),

        // Pending queue
        h('div', { class: 'pending-queue' },
          h('h3', {}, '📦 Pending Queue'),
          h('div', { class: 'queue-items', id: 'pending-queue-items' }),
          h('button', {
            id: 'execute-btn',
            class: 'btn btn-execute',
            disabled: true,
            onClick: executeQueue
          }, '⚡ Execute All Jobs')
        ),

        // Worker status
        h('h2', { id: 'workers-section' }, '👷 Worker Pool (4 Goroutines)'),
        h('div', { class: 'worker-grid', id: 'worker-grid' }),

        // Job queue
        h('div', { class: 'section-header' },
          h('h2', {}, '📋 Job Queue'),
          h('button', {
            id: 'clear-all-btn',
            class: 'btn btn-clear-all',
            disabled: true,
            onClick: clearAllCompleted
          }, '🗑️ Clear All Completed')
        ),
        h('div', { class: 'job-list', id: 'job-list' })
      )
    );
  },
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
    // Clean up any existing intervals when changing pages
    if (state.autoRefresh) {
      console.log('Cleaning up auto-refresh on page change');
      clearInterval(state.autoRefresh);
      state.autoRefresh = null;
    }

    el.innerHTML = '';
    el.appendChild(App(props));

    // Initial render for factorize page
    if (props.workers) {
      setTimeout(() => {
        renderWorkers();
        renderJobs();
        renderQueue();
      }, 0);
    }
  },

  initialPage: window.initialPage,

  progress: {
    delay: 250,
    color: '#16f4d0',
    includeCSS: true,
    showSpinner: true,
  },
});

// Cleanup on page unload
window.addEventListener('beforeunload', () => {
  if (state.autoRefresh) {
    clearInterval(state.autoRefresh);
  }
});

console.log('⚡ Go Concurrent Factorization initialized!');
