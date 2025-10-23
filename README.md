# Switchback

[![GitHub Release](https://img.shields.io/github/v/release/ge3224/switchback)](https://github.com/ge3224/switchback/releases)
[![No Dependencies](https://img.shields.io/badge/dependencies-0-brightgreen.svg)](https://github.com/ge3224/switchback)
[![GitHub License](https://img.shields.io/github/license/ge3224/switchback)](https://github.com/ge3224/switchback/blob/main/LICENSE)
[![CI](https://github.com/ge3224/switchback/actions/workflows/ci.yml/badge.svg)](https://github.com/ge3224/switchback/actions/workflows/ci.yml)

Build Single-Page Apps that integrate with your preferred server stack. Build in vanilla TypeScript—no framework adoption, zero dependencies.

Switchback is ~300 lines that handle link interception, form handling, and DOM updates. Read the source code in one sitting. Vendor it into your project, modify it as needed.

## Examples

Here are full-stack demos in different languages. Docker keeps them isolated for convenient experimentation. See [examples/demos/](examples/demos/).

- **C** - Optimistic updates for instant UX
- **C#** - Data visualization with LINQ query composition
- **Deno** - TypeScript type sharing between client and server
- **Erlang** - Server-side rendering with HTML morphing and stateful backend
- **Go** - Concurrent worker pools with true parallelism
- **OCaml** - Workflow states with compiler-enforced safety
- **PHP** - Server-side routing with SPA navigation
- **Rust** - Image processing pipeline with partial reloads and upload progress tracking
- **Zig** - Form handling with POST requests and server-side state
- **Assembly** - x86-64 assembly (because why not?)

## Philosophy

- **No client-side routing** - Your server owns the routes
- **No API needed** - Just return page data from controllers
- **Proven patterns** - Build with controllers and views
- **Framework agnostic** - Works with any backend

## Features

- Link interception (make `<a>` tags async)
- Form interception (make forms async)
- History management (back/forward buttons work)
- Progress indicators (XHR option for download progress)
- Scroll restoration (remembers scroll position)
- Partial reloads (only refresh specific props)
- Error handling (server errors, validation errors)
- SSR support (morph server-rendered HTML into the DOM)

## Installation

### Browser-ready bundle (no build step)

Download the latest release:

```html
<!-- ESM -->
<script type="module">
  import { newSwitchback } from './switchback.esm.js';
  // ...
</script>

<!-- Or UMD -->
<script src="./switchback.umd.js"></script>
<script>
  const { newSwitchback } = Switchback;
  // ...
</script>
```

Get bundles from [releases](https://github.com/ge3224/switchback/releases).

### Via git submodule (for TypeScript projects)

```bash
git submodule add https://github.com/ge3224/switchback vendor/switchback
```

Then import from `vendor/switchback`.

## Usage

### Client Setup

```typescript
import { newSwitchback } from './vendor/switchback';

// Your page components
const pages = {
  'Home': (props) => {
    const div = document.createElement('div');
    div.innerHTML = `<h1>${props.title}</h1>`;
    return div;
  },
};

// Initialize
newSwitchback({
  resolve: (name) => pages[name],
  setup: ({ el, App, props }) => {
    el.innerHTML = '';
    el.appendChild(App(props));
  },
  initialPage: window.initialPage,
});
```

### HTML Setup

```html
<!DOCTYPE html>
<html>
<head>
  <title>My App</title>
  <script>
    window.initialPage = <?= json_encode($page) ?>;
  </script>
</head>
<body>
  <div data-swbk-app></div>
  <script type="module" src="/app.js"></script>
</body>
</html>
```

### Server Setup

Your server returns JSON when the `X-Switchback` header is present:

**Java/Spring Boot example:**

```java
@GetMapping("/users/{id}")
public ResponseEntity<?> showUser(@PathVariable String id,
                                   @RequestHeader(value = "X-Switchback", required = false) String switchback) {
    User user = userService.findById(id);

    Map<String, Object> page = Map.of(
        "component", "Users/Show",
        "props", Map.of("user", user),
        "url", "/users/" + id
    );

    if (switchback != null) {
        return ResponseEntity.ok(page);  // Return JSON
    }

    return ResponseEntity.ok(renderHtml(page));  // Return HTML wrapper
}
```

### Component Examples

Add `data-swbk` to links and forms you want intercepted:

```typescript
// Intercepted link
const link = document.createElement('a');
link.href = '/users/123';
link.setAttribute('data-swbk', '');
link.textContent = 'View User';

// Intercepted form
const form = document.createElement('form');
form.action = '/users/123';
form.method = 'post';
form.setAttribute('data-swbk', '');

// Regular link (not intercepted)
const external = document.createElement('a');
external.href = 'https://example.com';
external.textContent = 'External Site';
```

## API

### `newSwitchback(config)`

Initialize the Switchback app.

```typescript
const app = newSwitchback({
  resolve: (name: string) => Promise<Component> | Component,
  setup: ({ el, App, props }) => void,
  initialPage?: Page,
  progress?: {
    delay?: number,
    color?: string,
    includeCSS?: boolean,
    showSpinner?: boolean,
  }
});
```

### `visit(url, options)`

Programmatic navigation.

```typescript
visit('/users/123', {
  method: 'get',                    // HTTP method
  data: { name: 'John' },           // Request data
  headers: {},                      // Additional headers
  replace: false,                   // Replace history entry
  preserveScroll: false,            // Keep scroll position
  preserveState: false,             // Merge with current state
  only: ['user'],                   // Partial reload (only fetch these props)
  useXhr: false,                    // Use XHR for progress tracking

  // Lifecycle callbacks
  onStart: () => {},
  onProgress: (e: ProgressEvent) => {},
  onSuccess: (page) => {},
  onError: (errors) => {},
  onFinish: () => {},
});
```

### `page()`

Get current page data.

```typescript
const currentPage = page();
console.log(currentPage.component, currentPage.props, currentPage.url);
```

### `reload(options)`

Reload current page.

```typescript
reload({ only: ['user'] }); // Partial reload
```

## Advanced Features

### Progress Indicator

Use XHR for download progress tracking:

```typescript
visit('/large-data', {
  useXhr: true,
  onProgress: (e) => {
    if (e.lengthComputable) {
      const percent = (e.loaded / e.total) * 100;
      console.log(`${percent}% loaded`);
    }
  }
});
```

### Partial Reloads

Only refresh specific props:

```typescript
visit('/users/123', {
  only: ['user'], // Only refresh 'user' prop, keep others
});
```

### Scroll Management

```typescript
// Preserve scroll on navigation
visit('/page', { preserveScroll: true });

// Use data attributes
const link = document.createElement('a');
link.href = '/page';
link.setAttribute('data-preserve-scroll', '');
link.textContent = 'Link';
```

## Inspiration

Switchback draws inspiration from [Inertia.js](https://inertiajs.com) but is designed specifically for vanilla TypeScript projects. Key differences:

- **No framework dependency** - Works with vanilla DOM or any rendering approach
- **Minimal** - ~300 lines you can audit vs thousands
- **Vendorable** - Git submodule, direct copy, or browser bundle
- **No adapters** - Just check a header and return JSON
- **Optional SSR** - Backend can send HTML directly, no Node.js process required

## License

MIT
