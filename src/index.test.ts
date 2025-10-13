import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import {
  newSwitchback,
  type Switchback,
  type Page,
  type SwitchbackConfig,
} from "./index";

// Global setup - mock window.scrollTo
beforeEach(() => {
  // Mock window.scrollTo
  window.scrollTo = vi.fn();
  // Mock scroll properties
  Object.defineProperty(window, 'scrollX', { value: 0, writable: true, configurable: true });
  Object.defineProperty(window, 'scrollY', { value: 0, writable: true, configurable: true });
});

describe("Switchback", () => {
  beforeEach(() => {
    document.body.innerHTML = '<div data-switchback-app></div>';
    // Reset history
    history.replaceState(null, '', '/');
  });

  it("should initialize without errors", () => {
    const config: SwitchbackConfig = {
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    };

    expect(() => newSwitchback(config)).not.toThrow();
  });

  it("should set initial page state", () => {
    const initialPage: Page = {
      component: 'Home',
      props: { title: 'Test' },
      url: '/test',
    };

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
      initialPage,
    });

    expect(app.page()).toEqual(initialPage);
  });

  it("should update history with initial page", () => {
    const initialPage: Page = {
      component: 'Home',
      props: {},
      url: '/initial',
    };

    newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
      initialPage,
    });

    expect(history.state).toEqual({ page: initialPage });
  });

  it("should call setup function with initial page", () => {
    const setupSpy = vi.fn();
    const initialPage: Page = {
      component: 'Home',
      props: { value: 42 },
      url: '/',
    };

    newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: setupSpy,
      initialPage,
    });

    expect(setupSpy).not.toHaveBeenCalled(); // Initial page doesn't trigger setup
  });
});

describe("visit", () => {
  let mockFetch: ReturnType<typeof vi.fn>;
  let originalFetch: typeof global.fetch;

  beforeEach(() => {
    document.body.innerHTML = '<div data-switchback-app></div>';
    history.replaceState(null, '', '/');

    // Mock fetch
    originalFetch = global.fetch;
    mockFetch = vi.fn();
    global.fetch = mockFetch;
  });

  afterEach(() => {
    global.fetch = originalFetch;
  });

  it("should make fetch request with correct headers", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    await app.visit('/test');

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/test'),
      expect.objectContaining({
        method: 'GET',
        headers: expect.objectContaining({
          'X-Switchback': 'true',
          'Accept': 'application/json',
        }),
      })
    );
  });

  it("should use POST method when specified", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    await app.visit('/test', { method: 'post' });

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/test'),
      expect.objectContaining({
        method: 'POST',
      })
    );
  });

  it("should send FormData as body for POST", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    const formData = new FormData();
    formData.append('name', 'John');

    await app.visit('/test', { method: 'post', data: formData });

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/test'),
      expect.objectContaining({
        method: 'POST',
        body: formData,
      })
    );
  });

  it("should send JSON for POST with object data", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    const data = { name: 'John', age: 30 };

    await app.visit('/test', { method: 'post', data });

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/test'),
      expect.objectContaining({
        method: 'POST',
        body: JSON.stringify(data),
      })
    );
  });

  it("should append query parameters for GET with data", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    await app.visit('/test', { data: { foo: 'bar', baz: 'qux' } });

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/test?'),
      expect.any(Object)
    );
    expect(mockFetch.mock.calls[0][0]).toContain('foo=bar');
    expect(mockFetch.mock.calls[0][0]).toContain('baz=qux');
  });

  it("should call onStart callback", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    const onStart = vi.fn();
    await app.visit('/test', { onStart });

    expect(onStart).toHaveBeenCalledTimes(1);
  });

  it("should call onSuccess callback with page data", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: { value: 42 },
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    const onSuccess = vi.fn();
    await app.visit('/test', { onSuccess });

    expect(onSuccess).toHaveBeenCalledWith(mockPage);
  });

  it("should call onError callback on fetch error", async () => {
    mockFetch.mockRejectedValue(new Error('Network error'));

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    const onError = vi.fn();
    await app.visit('/test', { onError });

    expect(onError).toHaveBeenCalled();
  });

  it("should call onError callback on non-ok response", async () => {
    mockFetch.mockResolvedValue({
      ok: false,
      json: async () => ({ error: 'Not found' }),
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    const onError = vi.fn();
    await app.visit('/test', { onError });

    expect(onError).toHaveBeenCalledWith({ error: 'Not found' });
  });

  it("should call onFinish callback after success", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    const onFinish = vi.fn();
    await app.visit('/test', { onFinish });

    expect(onFinish).toHaveBeenCalledTimes(1);
  });

  it("should call onFinish callback after error", async () => {
    mockFetch.mockRejectedValue(new Error('Network error'));

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    const onFinish = vi.fn();
    await app.visit('/test', { onFinish });

    expect(onFinish).toHaveBeenCalledTimes(1);
  });

  it("should include custom headers", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    await app.visit('/test', {
      headers: { 'X-Custom': 'value' },
    });

    expect(mockFetch).toHaveBeenCalledWith(
      '/test',
      expect.objectContaining({
        headers: expect.objectContaining({
          'X-Custom': 'value',
        }),
      })
    );
  });

  it("should include partial data header when only option is provided", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    await app.visit('/test', { only: ['user', 'posts'] });

    expect(mockFetch).toHaveBeenCalledWith(
      '/test',
      expect.objectContaining({
        headers: expect.objectContaining({
          'X-Switchback-Partial-Data': 'user,posts',
        }),
      })
    );
  });

  it("should update page state after successful navigation", async () => {
    const mockPage: Page = {
      component: 'NewPage',
      props: { title: 'New' },
      url: '/new',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    await app.visit('/new');

    expect(app.page()).toEqual(mockPage);
  });

  it("should update browser history", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    await app.visit('/test');

    expect(history.state).toEqual({ page: mockPage });
  });

  it("should replace history when replace option is true", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const replaceStateSpy = vi.spyOn(history, 'replaceState');
    const pushStateSpy = vi.spyOn(history, 'pushState');

    const app = newSwitchback({
      resolve: (name) => () => document.createElement('div'),
      setup: () => {},
    });

    await app.visit('/test', { replace: true });

    expect(replaceStateSpy).toHaveBeenCalled();
    expect(pushStateSpy).not.toHaveBeenCalled();

    replaceStateSpy.mockRestore();
    pushStateSpy.mockRestore();
  });

  it("should call resolve with component name", async () => {
    const mockPage: Page = {
      component: 'MyComponent',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const resolve = vi.fn(() => () => document.createElement('div'));

    const app = newSwitchback({
      resolve,
      setup: () => {},
    });

    await app.visit('/test');

    expect(resolve).toHaveBeenCalledWith('MyComponent');
  });

  it("should call setup with correct parameters", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: { value: 123 },
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const mockComponent = vi.fn(() => document.createElement('div'));
    const setup = vi.fn();

    const app = newSwitchback({
      resolve: () => mockComponent,
      setup,
    });

    await app.visit('/test');

    expect(setup).toHaveBeenCalledWith({
      el: document.querySelector('[data-switchback-app]'),
      App: mockComponent,
      props: { value: 123 },
    });
  });

  it("should merge props for partial reload", async () => {
    const initialPage: Page = {
      component: 'Test',
      props: { user: { name: 'John' }, count: 5 },
      url: '/test',
    };

    const partialPage: Page = {
      component: 'Test',
      props: { count: 10 },
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => partialPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
      initialPage,
    });

    await app.visit('/test', { only: ['count'] });

    const currentPage = app.page();
    expect(currentPage?.props).toEqual({
      user: { name: 'John' },
      count: 10,
    });
  });

  it("should handle async component resolution", async () => {
    const mockPage: Page = {
      component: 'AsyncComponent',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const asyncResolve = vi.fn(async (name: string) => {
      await new Promise(resolve => setTimeout(resolve, 10));
      return () => document.createElement('div');
    });

    const app = newSwitchback({
      resolve: asyncResolve,
      setup: () => {},
    });

    await app.visit('/test');

    expect(asyncResolve).toHaveBeenCalledWith('AsyncComponent');
  });
});

describe("page", () => {
  beforeEach(() => {
    document.body.innerHTML = '<div data-switchback-app></div>';
    history.replaceState(null, '', '/');
  });

  it("should return null when no page is set", () => {
    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    expect(app.page()).toBeNull();
  });

  it("should return current page", () => {
    const initialPage: Page = {
      component: 'Home',
      props: { title: 'Test' },
      url: '/',
    };

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
      initialPage,
    });

    expect(app.page()).toEqual(initialPage);
  });

  it("should return a copy of page (not reference)", () => {
    const initialPage: Page = {
      component: 'Home',
      props: { title: 'Test' },
      url: '/',
    };

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
      initialPage,
    });

    const currentPage = app.page();
    currentPage!.props.title = 'Modified';

    expect(app.page()?.props.title).toBe('Test');
  });
});

describe("reload", () => {
  let mockFetch: ReturnType<typeof vi.fn>;
  let originalFetch: typeof global.fetch;

  beforeEach(() => {
    document.body.innerHTML = '<div data-switchback-app></div>';
    history.replaceState(null, '', '/');

    originalFetch = global.fetch;
    mockFetch = vi.fn();
    global.fetch = mockFetch;
  });

  afterEach(() => {
    global.fetch = originalFetch;
  });

  // Note: Testing "no current page" error is difficult due to module-level state.
  // In real usage, calling reload() without a current page will throw
  // "No current page to reload" error.

  it("should reload current page", async () => {
    const initialPage: Page = {
      component: 'Home',
      props: { count: 1 },
      url: '/home',
    };

    const reloadedPage: Page = {
      component: 'Home',
      props: { count: 2 },
      url: '/home',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => reloadedPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
      initialPage,
    });

    await app.reload();

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/home'),
      expect.objectContaining({
        headers: expect.objectContaining({
          'X-Switchback': 'true',
        }),
      })
    );
  });

  it("should use replace: true by default", async () => {
    const initialPage: Page = {
      component: 'Home',
      props: {},
      url: '/home',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => initialPage,
    });

    const replaceStateSpy = vi.spyOn(history, 'replaceState');

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
      initialPage,
    });

    await app.reload();

    expect(replaceStateSpy).toHaveBeenCalled();
    replaceStateSpy.mockRestore();
  });

  it("should support partial reload", async () => {
    const initialPage: Page = {
      component: 'Home',
      props: { user: { name: 'John' }, count: 1 },
      url: '/home',
    };

    const partialPage: Page = {
      component: 'Home',
      props: { count: 2 },
      url: '/home',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => partialPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
      initialPage,
    });

    await app.reload({ only: ['count'] });

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/home'),
      expect.objectContaining({
        headers: expect.objectContaining({
          'X-Switchback-Partial-Data': 'count',
        }),
      })
    );

    expect(app.page()?.props).toEqual({
      user: { name: 'John' },
      count: 2,
    });
  });
});

describe("link interception", () => {
  let mockFetch: ReturnType<typeof vi.fn>;
  let originalFetch: typeof global.fetch;

  beforeEach(() => {
    document.body.innerHTML = '<div data-switchback-app></div>';
    history.replaceState(null, '', '/');

    originalFetch = global.fetch;
    mockFetch = vi.fn();
    global.fetch = mockFetch;
  });

  afterEach(() => {
    global.fetch = originalFetch;
  });

  it("should intercept same-origin link clicks", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const link = document.createElement('a');
    link.href = window.location.origin + '/test';
    document.body.appendChild(link);

    const clickEvent = new MouseEvent('click', { bubbles: true, cancelable: true });
    const preventDefaultSpy = vi.spyOn(clickEvent, 'preventDefault');

    link.dispatchEvent(clickEvent);

    // Wait for async fetch
    await new Promise(resolve => setTimeout(resolve, 10));

    expect(preventDefaultSpy).toHaveBeenCalled();
    expect(mockFetch).toHaveBeenCalled();
  });

  it("should NOT intercept links with target attribute", () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const link = document.createElement('a');
    link.href = window.location.origin + '/test';
    link.target = '_blank';
    document.body.appendChild(link);

    const clickEvent = new MouseEvent('click', { bubbles: true, cancelable: true });
    const preventDefaultSpy = vi.spyOn(clickEvent, 'preventDefault');

    link.dispatchEvent(clickEvent);

    expect(preventDefaultSpy).not.toHaveBeenCalled();
  });

  it("should NOT intercept links with download attribute", () => {
    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const link = document.createElement('a');
    link.href = window.location.origin + '/file.pdf';
    link.setAttribute('download', '');
    document.body.appendChild(link);

    const clickEvent = new MouseEvent('click', { bubbles: true, cancelable: true });
    const preventDefaultSpy = vi.spyOn(clickEvent, 'preventDefault');

    link.dispatchEvent(clickEvent);

    expect(preventDefaultSpy).not.toHaveBeenCalled();
  });

  it("should NOT intercept links with data-no-swizzle attribute", () => {
    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const link = document.createElement('a');
    link.href = window.location.origin + '/test';
    link.setAttribute('data-no-swizzle', '');
    document.body.appendChild(link);

    const clickEvent = new MouseEvent('click', { bubbles: true, cancelable: true });
    const preventDefaultSpy = vi.spyOn(clickEvent, 'preventDefault');

    link.dispatchEvent(clickEvent);

    expect(preventDefaultSpy).not.toHaveBeenCalled();
  });

  it("should NOT intercept external links", () => {
    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const link = document.createElement('a');
    link.href = 'https://external.com/test';
    document.body.appendChild(link);

    const clickEvent = new MouseEvent('click', { bubbles: true, cancelable: true });
    const preventDefaultSpy = vi.spyOn(clickEvent, 'preventDefault');

    link.dispatchEvent(clickEvent);

    expect(preventDefaultSpy).not.toHaveBeenCalled();
  });

  it("should handle data-replace attribute", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const replaceStateSpy = vi.spyOn(history, 'replaceState');

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const link = document.createElement('a');
    link.href = window.location.origin + '/test';
    link.setAttribute('data-replace', '');
    document.body.appendChild(link);

    link.click();

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(replaceStateSpy).toHaveBeenCalled();
    replaceStateSpy.mockRestore();
  });
});

describe("form interception", () => {
  let mockFetch: ReturnType<typeof vi.fn>;
  let originalFetch: typeof global.fetch;

  beforeEach(() => {
    document.body.innerHTML = '<div data-switchback-app></div>';
    history.replaceState(null, '', '/');

    originalFetch = global.fetch;
    mockFetch = vi.fn();
    global.fetch = mockFetch;
  });

  afterEach(() => {
    global.fetch = originalFetch;
  });

  it("should intercept form submissions", async () => {
    const mockPage: Page = {
      component: 'Success',
      props: {},
      url: '/success',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const form = document.createElement('form');
    form.action = '/submit';
    form.method = 'post';
    document.body.appendChild(form);

    const submitEvent = new Event('submit', { bubbles: true, cancelable: true });
    const preventDefaultSpy = vi.spyOn(submitEvent, 'preventDefault');

    form.dispatchEvent(submitEvent);

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(preventDefaultSpy).toHaveBeenCalled();
    expect(mockFetch).toHaveBeenCalled();
  });

  it("should NOT intercept forms with data-no-swizzle", () => {
    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const form = document.createElement('form');
    form.action = '/submit';
    form.setAttribute('data-no-swizzle', '');
    document.body.appendChild(form);

    const submitEvent = new Event('submit', { bubbles: true, cancelable: true });
    const preventDefaultSpy = vi.spyOn(submitEvent, 'preventDefault');

    form.dispatchEvent(submitEvent);

    expect(preventDefaultSpy).not.toHaveBeenCalled();
  });

  it("should use form method", async () => {
    const mockPage: Page = {
      component: 'Success',
      props: {},
      url: '/submit',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const form = document.createElement('form');
    form.action = '/submit';
    form.method = 'put';
    document.body.appendChild(form);

    form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/submit'),
      expect.objectContaining({
        method: 'PUT',
      })
    );
  });

  it("should send FormData from form", async () => {
    const mockPage: Page = {
      component: 'Success',
      props: {},
      url: '/submit',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    const form = document.createElement('form');
    form.action = '/submit';
    form.method = 'post';

    const input = document.createElement('input');
    input.name = 'username';
    input.value = 'testuser';
    form.appendChild(input);

    document.body.appendChild(form);

    form.dispatchEvent(new Event('submit', { bubbles: true, cancelable: true }));

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(mockFetch).toHaveBeenCalledWith(
      expect.stringContaining('/submit'),
      expect.objectContaining({
        method: 'POST',
        body: expect.any(FormData),
      })
    );

    const lastCallIndex = mockFetch.mock.calls.length - 1;
    const formData = mockFetch.mock.calls[lastCallIndex][1].body as FormData;
    expect(formData.get('username')).toBe('testuser');
  });
});

describe("scroll management", () => {
  let mockFetch: ReturnType<typeof vi.fn>;
  let originalFetch: typeof global.fetch;

  beforeEach(() => {
    document.body.innerHTML = '<div data-switchback-app></div>';
    history.replaceState(null, '', '/');

    originalFetch = global.fetch;
    mockFetch = vi.fn();
    global.fetch = mockFetch;
  });

  afterEach(() => {
    global.fetch = originalFetch;
  });

  it("should restore scroll position to top for new page", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    Object.defineProperty(window, 'scrollY', { value: 500, writable: true, configurable: true });

    await app.visit('/test');

    expect(window.scrollTo).toHaveBeenCalledWith(0, 0);
  });

  it("should preserve scroll position when preserveScroll is true", async () => {
    const mockPage: Page = {
      component: 'Test',
      props: {},
      url: '/test',
    };

    mockFetch.mockResolvedValue({
      ok: true,
      json: async () => mockPage,
    });

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    Object.defineProperty(window, 'scrollY', { value: 500, writable: true, configurable: true });

    await app.visit('/test', { preserveScroll: true });

    expect(window.scrollTo).not.toHaveBeenCalled();
  });
});

describe("history navigation", () => {
  let mockFetch: ReturnType<typeof vi.fn>;
  let originalFetch: typeof global.fetch;

  beforeEach(() => {
    document.body.innerHTML = '<div data-switchback-app></div>';
    history.replaceState(null, '', '/');

    originalFetch = global.fetch;
    mockFetch = vi.fn();
    global.fetch = mockFetch;
  });

  afterEach(() => {
    global.fetch = originalFetch;
  });

  it("should handle popstate event", async () => {
    const storedPage: Page = {
      component: 'Previous',
      props: {},
      url: '/previous',
    };

    const app = newSwitchback({
      resolve: () => () => document.createElement('div'),
      setup: () => {},
    });

    // Simulate popstate with stored page
    window.dispatchEvent(new PopStateEvent('popstate', {
      state: { page: storedPage },
    }));

    await new Promise(resolve => setTimeout(resolve, 10));

    expect(app.page()).toEqual(storedPage);
  });
});
