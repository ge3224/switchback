/**
 * Switchback - SPA navigation for vanilla TypeScript
 *
 * Zero-dependency library for building SPAs with traditional backend patterns.
 * Works seamlessly with just-jsx for rendering and simple-state for shared data.
 */

/** Page response from server */
export interface Page {
  component: string;
  props: Record<string, any>;
  url: string;
  version?: string;
}

/** Navigation options */
export interface VisitOptions {
  method?: 'get' | 'post' | 'put' | 'patch' | 'delete';
  data?: Record<string, any> | FormData;
  headers?: Record<string, string>;
  replace?: boolean;
  preserveScroll?: boolean;
  preserveState?: boolean;
  only?: string[];
  onStart?: () => void;
  onProgress?: (progress: ProgressEvent) => void;
  onSuccess?: (page: Page) => void;
  onError?: (errors: any) => void;
  onFinish?: () => void;
  useXhr?: boolean;
}

/** App configuration */
export interface SwitchbackConfig {
  resolve: (name: string) => Promise<any> | any;
  setup: (options: { el: Element; App: any; props: any }) => void;
  progress?: {
    delay?: number;
    color?: string;
    includeCSS?: boolean;
    showSpinner?: boolean;
  };
  initialPage?: Page;
}

export interface Switchback {
  visit(url: string, options?: VisitOptions): Promise<void>;
  page(): Page | null;
  reload(options?: Omit<VisitOptions, 'method'>): Promise<void>;
}

export function newSwitchback(config: SwitchbackConfig): Switchback {
  let currentPage: Page | null = config.initialPage || null;
  let swapInProgress = false;
  const scrollPositions: Record<string, { x: number; y: number }> = {};

  async function visitWithFetch(
    url: string,
    method: string,
    headers: Record<string, string>,
    options: VisitOptions
  ): Promise<void> {
    try {
      const isGetRequest = method === 'get';
      const fetchUrl = isGetRequest && options.data
        ? `${url}?${new URLSearchParams(options.data as Record<string, string>)}`
        : url;

      const response = await fetch(fetchUrl, {
        method: method.toUpperCase(),
        headers,
        body: !isGetRequest && options.data
          ? options.data instanceof FormData ? options.data : JSON.stringify(options.data)
          : undefined,
      });

      if (!response.ok) {
        const errors = await response.json().catch(() => ({ message: 'Request failed' }));
        options.onError?.(errors);
        options.onFinish?.();
        return;
      }

      const page: Page = await response.json();
      await swapPage(page, options);
      options.onSuccess?.(page);
    } catch (error) {
      options.onError?.(error);
    } finally {
      options.onFinish?.();
    }
  }

  function visitWithXhr(
    url: string,
    headers: Record<string, string>,
    options: VisitOptions
  ): Promise<void> {
    return new Promise(function promiseExecutor(resolve, reject) {
      const xhr = new XMLHttpRequest();
      xhr.open('GET', url);

      for (const [key, value] of Object.entries(headers)) {
        xhr.setRequestHeader(key, value);
      }

      xhr.onprogress = function onProgress(e) {
        options.onProgress?.(e);
      };

      xhr.onload = async function onLoad() {
        if (xhr.status >= 200 && xhr.status < 300) {
          try {
            const page: Page = JSON.parse(xhr.responseText);
            await swapPage(page, options);
            options.onSuccess?.(page);
            resolve();
          } catch (error) {
            options.onError?.(error);
            reject(error);
          }
        } else {
          const errors = tryParseJson(xhr.responseText) || { message: 'Request failed' };
          options.onError?.(errors);
          reject(errors);
        }
        options.onFinish?.();
      };

      xhr.onerror = function onError() {
        options.onError?.(new Error('Network error'));
        options.onFinish?.();
        reject(new Error('Network error'));
      };

      xhr.send();
    });
  }

  async function swapPage(page: Page, options: VisitOptions): Promise<void> {
    if (swapInProgress) return;
    swapInProgress = true;

    // Save scroll position before swap
    if (currentPage && !options.preserveScroll) {
      scrollPositions[currentPage.url] = { x: window.scrollX, y: window.scrollY };
    }

    // Merge props if partial reload
    const mergedPage = options.only && currentPage
      ? { ...page, props: { ...currentPage.props, ...page.props } }
      : page;

    currentPage = mergedPage;

    // Update history
    history[options.replace ? 'replaceState' : 'pushState']({ page: mergedPage }, '', mergedPage.url);

    // Resolve component and render
    const Component = await Promise.resolve(config.resolve(mergedPage.component));
    const el = document.querySelector('[data-swbk-app]');
    if (el) config.setup({ el, App: Component, props: mergedPage.props });

    // Restore scroll position
    if (!options.preserveScroll) {
      const savedPosition = scrollPositions[mergedPage.url];
      window.scrollTo(savedPosition?.x ?? 0, savedPosition?.y ?? 0);
    }

    swapInProgress = false;
  }

  function setupInterceptors(): void {
    // Intercept link clicks
    document.addEventListener('click', function onClick(e) {
      const target = e.target as HTMLElement;
      const link = target.closest('a') as HTMLAnchorElement | null;

      if (!link || !shouldIntercept(link)) return;

      e.preventDefault();
      visit(link.href, {
        replace: link.hasAttribute('data-replace'),
        preserveScroll: link.hasAttribute('data-preserve-scroll'),
      });
    });

    // Intercept form submits
    document.addEventListener('submit', function onSubmit(e) {
      const form = e.target as HTMLFormElement;

      if (!shouldInterceptForm(form)) return;

      e.preventDefault();
      const formData = new FormData(form);
      const method = (form.getAttribute('method') || 'post').toLowerCase() as VisitOptions['method'];

      visit(form.action || window.location.href, {
        method,
        data: formData,
        replace: form.hasAttribute('data-replace'),
        preserveScroll: form.hasAttribute('data-preserve-scroll'),
      });
    });
  }

  function setupHistoryListener(): void {
    window.addEventListener('popstate', function onPopState(e) {
      if (e.state?.page) {
        swapPage(e.state.page, { preserveScroll: true });
      }
    });
  }

  function visit(url: string, options: VisitOptions = {}): Promise<void> {
    const method = options.method || 'get';
    const headers = {
      'X-Switchback': 'true',
      'Accept': 'application/json',
      ...(options.only && { 'X-Switchback-Partial-Data': options.only.join(',') }),
      ...options.headers,
    };

    options.onStart?.();
    return options.useXhr && method === 'get' && options.onProgress
      ? visitWithXhr(url, headers, options)
      : visitWithFetch(url, method, headers, options);
  }

  function page(): Page | null {
    return currentPage && { ...currentPage, props: { ...currentPage.props } };
  }

  function reload(options?: Omit<VisitOptions, 'method'>): Promise<void> {
    if (!currentPage) throw new Error('No current page to reload');
    return visit(currentPage.url, { ...options, replace: true });
  }

  // Initialize
  if (currentPage) history.replaceState({ page: currentPage }, '', currentPage.url);
  setupInterceptors();
  setupHistoryListener();

  return { visit, page, reload };
}

function shouldIntercept(link: HTMLAnchorElement): boolean {
  return link.hasAttribute('data-swbk') && !!link.href && !link.target &&
    !link.hasAttribute('download') && link.protocol === window.location.protocol &&
    link.host === window.location.host;
}

function shouldInterceptForm(form: HTMLFormElement): boolean {
  return form.hasAttribute('data-swbk');
}

function tryParseJson(text: string): any {
  try {
    return JSON.parse(text);
  } catch {
    return null;
  }
}
