# Switchback Examples

Interactive examples demonstrating Switchback's single-page app features.

## Running Examples

```bash
pnpm install
pnpm dev
```

Then open http://localhost:5173/examples

## What's Demonstrated

- **Link Interception**: Click any link - no full page reload!
- **Mock Backend**: Simulates server responses with JSON
- **Flash Messages**: Success/error notifications
- **Multiple Pages**: Home, Users list, User detail, About
- **Instant Navigation**: SPA speed with server-side routing patterns

## How It Works

1. Switchback intercepts all `<a>` clicks
2. Fetches page data from "server" (mocked in this demo)
3. Renders new page component
4. Updates browser history
5. All without a full page reload!

## Real Backend Integration

In a real app, your server would:

```php
// Detect Switchback request
if (request()->header('X-Switchback')) {
    return response()->json([
        'component' => 'Users/Show',
        'props' => ['user' => $user],
        'url' => "/users/{$id}",
    ]);
}

// Otherwise return full HTML
return view('app', ['page' => $page]);
```

See `server-example.php` for a complete PHP example.
