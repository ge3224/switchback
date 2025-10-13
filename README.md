# Switchback

Build single-page apps with your preferred server stack. Zero dependencies, vendorable, and auditable.

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
git submodule add https://github.com/yourusername/switchback vendor/switchback
```

### Via npm

```bash
npm install switchback
```

## Usage

### Client Setup

Works beautifully with [just-jsx](https://github.com/yourusername/just-jsx) and [simple-state](https://github.com/yourusername/simple-state):

```typescript
import { newSwitchback } from './vendor/switchback/src/index.ts';
import { createDomElement } from './vendor/just-jsx/src/index.ts';

// Your page components
const pages = {
  'Home': (props: any) => <div><h1>{props.title}</h1></div>,
  'Users/Show': (props: any) => <div><h1>{props.user.name}</h1></div>,
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
const UserList = ({ users }) => (
  <div>
    <h1>Users</h1>
    {users.map(user => (
      <a href={`/users/${user.id}`}>{user.name}</a>
    ))}
  </div>
);

// Forms - automatically intercepted
const UserEdit = ({ user }) => (
  <form action={`/users/${user.id}`} method="post">
    <input name="name" value={user.name} />
    <button type="submit">Save</button>
  </form>
);

// Opt-out with data-no-swizzle
const ExternalLink = () => (
  <a href="https://example.com" data-no-swizzle>External</a>
);
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
<a href="/page" data-preserve-scroll>Link</a>
```

### Integration with simple-state

```typescript
import { newSimpleState } from './vendor/simple-state/src/index.ts';

const flashState = newSimpleState({ message: null });

const app = newSwitchback({
  resolve: (name) => pages[name],
  setup: ({ el, App, props }) => {
    // Update shared state on page load
    if (props.flash) {
      flashState.set({ message: props.flash });
    }

    el.innerHTML = '';
    el.appendChild(App(props));
  },
});
```

## Why Switchback?

- **Simple**: No complex client-side routing or state management required
- **Familiar**: Use server-side patterns you already know
- **Fast**: Page transitions without full page reloads
- **Auditable**: ~200 lines of readable TypeScript
- **Zero deps**: No external dependencies to audit
- **Vendorable**: Easy to vendor via git submodules
- **Flexible**: Works with any backend framework

## Inspiration

Switchback draws inspiration from [Inertia.js](https://inertiajs.com) but is designed specifically for vanilla TypeScript projects. Key differences:

- **No framework dependency** - Works with just-jsx instead of requiring React/Vue/Svelte
- **Minimal** - ~300 lines you can audit vs thousands
- **Vendorable** - Designed to be vendored via git submodules
- **Server-agnostic** - Works with any backend that can return JSON

The `X-Switchback` header is compatible with the Inertia pattern, so you can adapt Inertia server-side adapters if needed.

## License

MIT
