# Switchback

[![GitHub Release](https://img.shields.io/github/v/release/ge3224/switchback)](https://github.com/ge3224/switchback/releases)
[![No Dependencies](https://img.shields.io/badge/dependencies-0-brightgreen.svg)](https://github.com/ge3224/switchback)
[![GitHub License](https://img.shields.io/github/license/ge3224/switchback)](https://github.com/ge3224/switchback/blob/main/LICENSE)
[![CI](https://github.com/ge3224/switchback/actions/workflows/ci.yml/badge.svg)](https://github.com/ge3224/switchback/actions/workflows/ci.yml)

Build Single Page Applications around your server stack. Build in **vanilla TypeScript**, no framework lock-in, no supply-chain vulnerabilities.

Switchback is ~300 lines meant to be vendored. Audit the codebase before you finish your first cup of coffee, customize it, happy trails.

## Examples

Here are some pairings you can spin up in a container to try out.

* [**Zig**](examples/demos/zig/) - Backend owns form routing and state, frontend delivers app-like UX
* [**Erlang**](examples/demos/erlang/) - Server-side rendering with HTML morphing and stateful back-end
* [**Go**](examples/demos/go/) - Real-time visualization of a concurrent worker pool processing CPU-intensive tasks
* [**Rust**](examples/demos/rust/) - Track bulk photos as they're swiftly processed in parallel
* [**OCaml**](examples/demos/ocaml/) - Workflow states with compiler-enforced safety
* [**C**](examples/demos/c/) - Pure C99 multi-threaded server with optimistic updates
* [**C#**](examples/demos/csharp/) - ASP.NET minimal APIs with typed data contracts
* [**Deno**](examples/demos/deno/) - Modern TypeScript runtime with type sharing between server and client
* [**PHP**](examples/demos/php/) - Persistent navigation with backend-controlled routing in vanilla PHP
* [**Assembly**](examples/demos/asm/) - x86-64 assembly (because why not?)

## Features

Your server owns routing and returns JSON for async requests, HTML for initial page loads. The client intercepts navigation and renders components. 

- Link interception (make `<a>` tags async)
- Form interception (make forms async)
- History management (back/forward buttons work)
- Progress indicators (XHR option for download progress)
- Scroll restoration (remembers scroll position)
- Partial reloads (only refresh specific props)
- Error handling (server errors, validation errors)
- SSR support (morph server-rendered HTML into the DOM)

## Installation

**Pre-built bundles** (no build step required):

Download ESM or UMD from [releases](https://github.com/ge3224/switchback/releases)

```html
<script type="module">
  import { newSwitchback } from './switchback.esm.js';
</script>
```

TypeScript source:

Download source from [releases](https://github.com/ge3224/switchback/releases) and import directly

Vendor with git:

```bash
git submodule add https://github.com/ge3224/switchback vendor/switchback
```

## Usage

Switchback coordinates between your server and client. The server detects async requests and returns JSON. The client intercepts links/forms and handles navigation.

### Server Side

```rust
// Rust example using Axum web framework
// Check for X-Switchback header
let is_switchback = req.headers().get("X-Switchback").is_some();

// Build page response
let page = json!({
    "component": "Home",
    "props": {
        "stats": {
            "users": user_count
        }
    },
    "url": "/"
});

// Return JSON for Switchback, HTML otherwise
if is_switchback {
    Json(page).into_response()
} else {
    Html(render_html(&page)).into_response()
}
```

This pattern can be implemented easily in your favorite language. Check the [demos](examples/demos/) for examples in Go, Rust, C, OCaml, Erlang, and more.

### Client Side

Define your page components and initialize Switchback:

```typescript
import { newSwitchback } from 'switchback';

// Simple DOM helper
function h(tag, props, ...children) {
  const el = document.createElement(tag);
  Object.assign(el, props);
  children.flat().forEach(child => {
    el.appendChild(
      typeof child === 'string'
        ? document.createTextNode(child)
        : child
    );
  });
  return el;
}

// Define page components
const pages = {
  'Home': (props) => h('div', {},
    h('h1', {}, 'Form Demo'),
    h('p', {}, `Submissions: ${props.stats.submissions}`)
  ),

  'Success': (props) => h('div', {},
    h('h1', {}, 'Success!'),
    h('p', {}, `Submitted: ${props.submitted.name}`)
  ),
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

### Mark Elements for Async Navigation

Add `data-swbk` to links and forms:

```html
<!-- Links navigate without page reload -->
<a href="/users" data-swbk>View Users</a>

<!-- Forms submit asynchronously -->
<form action="/" method="post" data-swbk>
  <input name="name" type="text" required>
  <input name="email" type="email" required>
  <button type="submit">Submit</button>
</form>
```

That's it. Your server returns JSON for `X-Switchback` requests, your client renders components, and Switchback handles the rest.

See the [demos](examples/demos/) for complete working examples.

## Inspiration

Switchback draws from [Inertia.js](https://inertiajs.com/) philosophy while staying framework-independent and highly auditable.

## License

MIT
