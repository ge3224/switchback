# Switchback

[![Version](https://img.shields.io/badge/version-0.1.0-blue.svg)](https://github.com/ge3224/switchback)
[![No Dependencies](https://img.shields.io/badge/dependencies-0-brightgreen.svg)](https://github.com/ge3224/switchback)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![CI](https://img.shields.io/badge/CI-passing-brightgreen.svg)](https://github.com/ge3224/switchback/actions)

Build single-page apps with your preferred server stack.

**Zero dependencies. Vendorable. Auditable. ~300 lines.**

## Philosophy

Switchback lets you create fully client-side rendered SPAs while keeping your familiar backend workflow. It works by:

- **No client-side routing** - Your server owns the routes
- **No API needed** - Just return page data from controllers
- **Traditional patterns** - Build controllers and views like always
- **Framework agnostic** - Works with any backend (Laravel, Rails, Express, Django, etc.)

## Features

- Link interception (make `<a>` tags async)
- Form interception (make forms async)
- History management (back/forward buttons work)
- Progress indicators (XHR option for download progress)
- Scroll restoration (remembers scroll position)
- Partial reloads (only refresh specific props)
- Error handling (server errors, validation errors)

## Installation

### Via git submodule (recommended for vendoring)

```bash
git submodule add https://github.com/ge3224/switchback vendor/switchback
```

## Usage

### Client Setup

```typescript
import { newSwitchback } from './vendor/switchback/src/index.ts';

// Your page components
const pages = {
  'Home': (props: any) => {
    const div = document.createElement('div');
    const h1 = document.createElement('h1');
    h1.textContent = props.title;
    div.appendChild(h1);
    return div;
  },
  'Users/Show': (props: any) => {
    const div = document.createElement('div');
    const h1 = document.createElement('h1');
    h1.textContent = props.user.name;
    div.appendChild(h1);
    return div;
  },
};

// Initialize Switchback
const app = newSwitchback({
  // Resolve component by name
  resolve: (name: string) => pages[name],

  // Setup/render function
  setup: ({ el, App, props }) => {
    el.innerHTML = '';
    el.appendChild(App(props));
  },

  // Optional: Pass initial page data from server
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
    // Pass initial page data from server
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

Your server returns JSON when `X-Switchback` header is present:

**PHP/Laravel example:**

```php
Route::get('/users/{id}', function ($id) {
    $user = User::find($id);

    if (request()->header('X-Switchback')) {
        return response()->json([
            'component' => 'Users/Show',
            'props' => ['user' => $user],
            'url' => "/users/{$id}",
        ]);
    }

    return view('app', [
        'page' => [
            'component' => 'Users/Show',
            'props' => ['user' => $user],
            'url' => "/users/{$id}",
        ]
    ]);
});
```

**Express example:**

```javascript
app.get('/users/:id', (req, res) => {
    const user = { name: 'John', email: 'john@example.com' };

    if (req.headers['x-switchback']) {
        return res.json({
            component: 'Users/Show',
            props: { user },
            url: `/users/${req.params.id}`,
        });
    }

    res.render('app', { page: { component: 'Users/Show', props: { user }, url: req.url } });
});
```

### Component Examples

Links and forms work automatically:

```typescript
// Links - no special handling needed
const UserList = ({ users }) => {
  const div = document.createElement('div');
  const h1 = document.createElement('h1');
  h1.textContent = 'Users';
  div.appendChild(h1);

  users.forEach(user => {
    const a = document.createElement('a');
    a.href = `/users/${user.id}`;
    a.textContent = user.name;
    div.appendChild(a);
  });

  return div;
};

// Forms - automatically intercepted
const UserEdit = ({ user }) => {
  const form = document.createElement('form');
  form.action = `/users/${user.id}`;
  form.method = 'post';

  const input = document.createElement('input');
  input.name = 'name';
  input.value = user.name;

  const button = document.createElement('button');
  button.type = 'submit';
  button.textContent = 'Save';

  form.appendChild(input);
  form.appendChild(button);
  return form;
};

// Opt-out with data-no-swizzle
const ExternalLink = () => {
  const a = document.createElement('a');
  a.href = 'https://example.com';
  a.setAttribute('data-no-swizzle', '');
  a.textContent = 'External';
  return a;
};
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

## Examples

See [examples/demos/](examples/demos/) for full-stack integration examples with different server stacks:

- **PHP** - Vanilla PHP with no framework dependencies
- **Deno** - TypeScript type sharing between client and server
- **Go** - Concurrent worker pools with true parallelism
- **Rust** - Embedded SQLite database with type safety
- **Erlang** - Actor model with real-time chat
- **Zig** - Blazingly fast with zero dependencies
- **C** - Optimistic updates for instant UX

Each demo includes Docker setup for easy testing.

## Why Switchback?

- **Simple**: No complex client-side routing or state management required
- **Familiar**: Use server-side patterns you already know
- **Fast**: Page transitions without full page reloads
- **Auditable**: ~300 lines of readable TypeScript
- **Zero deps**: No external dependencies to audit
- **Vendorable**: Easy to vendor via git submodules
- **Flexible**: Works with any backend framework

## Inspiration

Switchback draws inspiration from [Inertia.js](https://inertiajs.com) but is designed specifically for vanilla TypeScript projects. Key differences:

- **No framework dependency** - Works with vanilla DOM or any rendering approach
- **Minimal** - ~300 lines you can audit vs thousands
- **Vendorable** - Designed to be vendored via git submodules
- **Server-agnostic** - Works with any backend that can return JSON

The `X-Switchback` header is compatible with the Inertia pattern, so you can adapt Inertia server-side adapters if needed.

## License

MIT
