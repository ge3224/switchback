/**
 * Client-side app for Erlang SSR demo
 * Server renders HTML, Switchback morphs it into the DOM
 */

import { newSwitchback, type Page } from '../../../src/index.ts';

// Extend window to include initialPage
declare global {
  interface Window {
    initialPage: Page | undefined;
  }
}

// Track last viewed article in sessionStorage
function trackArticleView() {
  const match = window.location.pathname.match(/^\/articles\/(\d+)$/);
  if (match) {
    const articleId = match[1];
    sessionStorage.setItem('lastViewedArticleId', articleId);
  }
}

// Apply viewed styling to the last viewed article
let isApplyingStyling = false;

function applyViewedStyling() {
  if (isApplyingStyling) return;
  isApplyingStyling = true;

  const lastViewedId = sessionStorage.getItem('lastViewedArticleId');

  if (!lastViewedId) {
    isApplyingStyling = false;
    return;
  }

  // Only apply on home page
  if (window.location.pathname !== '/') {
    isApplyingStyling = false;
    return;
  }

  // Find all article rows and remove any existing viewed styling
  const allRows = document.querySelectorAll('.article-row');

  if (allRows.length === 0) {
    isApplyingStyling = false;
    return;
  }

  allRows.forEach(row => {
    row.classList.remove('viewed');
    const badge = row.querySelector('.viewed-badge');
    if (badge) badge.remove();
  });

  // Find the article link that matches the last viewed ID
  const articleLink = document.querySelector(`a[href="/articles/${lastViewedId}"]`);
  if (articleLink) {
    const articleRow = articleLink.closest('.article-row');
    if (articleRow) {
      articleRow.classList.add('viewed');

      // Add the viewed badge
      const titleDiv = articleRow.querySelector('.article-title');
      if (titleDiv && !titleDiv.querySelector('.viewed-badge')) {
        const badge = document.createElement('span');
        badge.className = 'viewed-badge';
        badge.textContent = '← Last viewed';
        titleDiv.appendChild(badge);
      }
    }
  }

  isApplyingStyling = false;
}

// Set up MutationObserver to watch for DOM changes
let mutationTimeout: number | null = null;

function setupDOMObserver() {
  const appContainer = document.querySelector('[data-swbk-app]');
  if (!appContainer) return;

  const observer = new MutationObserver((mutations) => {
    // Check for childList changes only (ignore our own attribute/class changes)
    const hasChildListChange = mutations.some(mutation => mutation.type === 'childList');
    if (!hasChildListChange) return;

    // Debounce: only run after mutations have stopped for 100ms
    if (mutationTimeout) clearTimeout(mutationTimeout);
    mutationTimeout = window.setTimeout(() => {
      trackArticleView();
      applyViewedStyling();
    }, 100);
  });

  observer.observe(appContainer, {
    childList: true,
    subtree: true,
    attributes: false,
  });
}

// Initialize Switchback with SSR mode
const app = newSwitchback({
  resolve: () => null,
  setup: () => {},
  initialPage: window.initialPage,
  progress: {
    delay: 250,
    color: '#b794f6',
    includeCSS: true,
    showSpinner: true,
  },
});

// Set up DOM observer to handle all navigation
setupDOMObserver();

// Listen to popstate events (browser back/forward button)
window.addEventListener('popstate', () => {
  // Wait for Switchback to finish morphing the DOM
  setTimeout(() => {
    trackArticleView();
    applyViewedStyling();
  }, 150);
});

// Apply styling on initial page load
trackArticleView();
applyViewedStyling();
