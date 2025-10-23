/**
 * Switchback - Single-page apps with your preferred server stack
 *
 * Zero-dependency library for building SPAs with your preferred backend.
 * Works seamlessly with just-jsx for rendering and simple-state for shared data.
 */

/** Page response from server */
export interface Page {
  component: string;
  props: Record<string, any>;
  url: string;
  version?: string;
  html?: string; // Server-rendered HTML for SSR
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
      const isGet = method === 'get';
      const fetchUrl = isGet && options.data ? `${url}?${new URLSearchParams(options.data as Record<string, string>)}` : url;
      const response = await fetch(fetchUrl, {
        method: method.toUpperCase(),
        headers,
        body: !isGet && options.data ? (options.data instanceof FormData ? options.data : JSON.stringify(options.data)) : undefined,
      });

      if (!response.ok) {
        options.onError?.(await response.json().catch(() => ({ message: 'Request failed' })));
        return options.onFinish?.();
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

  function visitWithXhr(url: string, headers: Record<string, string>, options: VisitOptions): Promise<void> {
    return new Promise(function promiseExecutor(resolve, reject) {
      const xhr = new XMLHttpRequest();
      xhr.open('GET', url);
      for (const key in headers) {
        xhr.setRequestHeader(key, headers[key]);
      }
      xhr.onprogress = function onProgress(e) { options.onProgress?.(e); };
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
        const error = new Error('Network error');
        options.onError?.(error);
        options.onFinish?.();
        reject(error);
      };
      xhr.send();
    });
  }

  function morphElement(fromEl: Element, toEl: Element): void {
    // Update attributes
    const fromAttrs = fromEl.attributes;
    const toAttrs = toEl.attributes;
    for (let i = fromAttrs.length - 1; i >= 0; i--) {
      if (!toEl.hasAttribute(fromAttrs[i].name)) fromEl.removeAttribute(fromAttrs[i].name);
    }
    for (let i = 0; i < toAttrs.length; i++) {
      const attr = toAttrs[i];
      if (fromEl.getAttribute(attr.name) !== attr.value) fromEl.setAttribute(attr.name, attr.value);
    }

    // Morph children
    const fromChildren = Array.from(fromEl.childNodes);
    const toChildren = Array.from(toEl.childNodes);
    let fromIdx = 0, toIdx = 0;

    while (toIdx < toChildren.length) {
      const toNode = toChildren[toIdx];
      const fromNode = fromChildren[fromIdx];

      if (!fromNode) {
        fromEl.appendChild(toNode.cloneNode(true));
        toIdx++;
        continue;
      }

      if (toNode.nodeType === 3 && fromNode.nodeType === 3) {
        if (fromNode.nodeValue !== toNode.nodeValue) fromNode.nodeValue = toNode.nodeValue;
        fromIdx++; toIdx++;
        continue;
      }

      if (toNode.nodeType === 1 && fromNode.nodeType === 1) {
        if ((toNode as Element).tagName === (fromNode as Element).tagName) {
          morphElement(fromNode as Element, toNode as Element);
          fromIdx++; toIdx++;
          continue;
        }
      }

      fromEl.replaceChild(toNode.cloneNode(true), fromNode);
      fromIdx++; toIdx++;
    }

    while (fromIdx < fromChildren.length) fromEl.removeChild(fromChildren[fromIdx++]);
  }

  async function swapPage(page: Page, options: VisitOptions): Promise<void> {
    if (swapInProgress) return;
    swapInProgress = true;

    if (currentPage && !options.preserveScroll) {
      scrollPositions[currentPage.url] = { x: window.scrollX, y: window.scrollY };
    }

    const mergedPage = options.only && currentPage ? { ...page, props: { ...currentPage.props, ...page.props } } : page;
    currentPage = mergedPage;
    history[options.replace ? 'replaceState' : 'pushState']({ page: mergedPage }, '', mergedPage.url);

    const el = document.querySelector('[data-swbk-app]');
    if (!el) return void (swapInProgress = false);

    if (mergedPage.html) {
      const template = document.createElement('template');
      template.innerHTML = mergedPage.html.trim();
      const newContent = template.content.firstElementChild;
      if (newContent) {
        el.firstElementChild ? morphElement(el.firstElementChild, newContent) : el.appendChild(newContent);
      }
    } else {
      const Component = await Promise.resolve(config.resolve(mergedPage.component));
      config.setup({ el, App: Component, props: mergedPage.props });
    }

    if (!options.preserveScroll) {
      const pos = scrollPositions[mergedPage.url];
      window.scrollTo(pos?.x ?? 0, pos?.y ?? 0);
    }

    swapInProgress = false;
  }

  function setupInterceptors(): void {
    document.addEventListener('click', function onClick(e) {
      const link = (e.target as HTMLElement).closest('a') as HTMLAnchorElement | null;
      if (!link || !shouldIntercept(link)) return;
      e.preventDefault();
      visit(link.href, {
        replace: link.hasAttribute('data-replace'),
        preserveScroll: link.hasAttribute('data-preserve-scroll'),
      });
    });

    document.addEventListener('submit', function onSubmit(e) {
      const form = e.target as HTMLFormElement;
      if (!shouldInterceptForm(form)) return;
      e.preventDefault();
      visit(form.action || window.location.href, {
        method: (form.getAttribute('method') || 'post').toLowerCase() as VisitOptions['method'],
        data: new FormData(form),
        replace: form.hasAttribute('data-replace'),
        preserveScroll: form.hasAttribute('data-preserve-scroll'),
      });
    });
  }

  function setupHistoryListener(): void {
    window.addEventListener('popstate', function onPopState(e) {
      if (e.state?.page) swapPage(e.state.page, { preserveScroll: true });
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
  setupInterceptors();
  setupHistoryListener();

  if (currentPage) {
    history.replaceState({ page: currentPage }, '', currentPage.url);
    const el = document.querySelector('[data-swbk-app]');
    if (el) {
      if (currentPage.html) {
        const template = document.createElement('template');
        template.innerHTML = currentPage.html.trim();
        const content = template.content.firstElementChild;
        if (content) el.firstElementChild ? morphElement(el.firstElementChild, content) : el.appendChild(content);
      } else {
        Promise.resolve(config.resolve(currentPage.component)).then(function setupComponent(Component) {
          config.setup({ el, App: Component, props: currentPage!.props });
        });
      }
    }
  }

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
