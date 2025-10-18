# PHP Demo - Switchback Integration

A minimal example showing how to integrate Switchback with vanilla PHP. No frameworks needed!

## What's Included

- **index.php** - Simple PHP router that detects `X-Switchback` header and returns JSON
- **app.ts** - Client-side Switchback app (TypeScript)
- **vite.config.ts** - Bundles app.ts + Switchback into single JS file
- **Docker setup** - One-command startup with PHP built-in server

## Try It Out

Want to see vanilla PHP + Switchback without setting up PHP locally?

```bash
cd examples/demos/php
docker-compose up
```

Open http://localhost:8000

## Running Natively

To run this demo with a local PHP installation, see the [PHP installation guide](https://www.php.net/manual/en/install.php). Most systems already have PHP installed - check with `php --version`.

## How It Works

1. **Initial Request**: Browser requests `/` → PHP returns full HTML with `window.initialPage`
2. **Switchback Request**: User clicks link → Switchback adds `X-Switchback` header → PHP returns JSON
3. **Client Rendering**: Switchback receives JSON, swaps page component, updates URL

```php
// Detect Switchback request
$isSwitchback = isset($_SERVER['HTTP_X_SWITCHBACK']);

if ($isSwitchback) {
    header('Content-Type: application/json');
    echo json_encode($page);
    exit;
}

// Otherwise return full HTML
?>
<!DOCTYPE html>
...
```

## Key Features Demonstrated

- ✅ Simple routing with `$_SERVER['REQUEST_URI']`
- ✅ JSON responses for Switchback requests
- ✅ Full HTML for initial page load
- ✅ No framework dependencies
- ✅ Works with PHP's built-in server

## File Structure

```
php/
├── index.php           # Backend router
├── app.js             # Frontend Switchback app
├── Dockerfile         # Docker image
├── docker-compose.yml # Docker setup
└── README.md          # This file
```

## Extending This Example

### Add a New Route

In `index.php`:

```php
elseif ($uri === '/posts' && $method === 'GET') {
    $page = respond('Posts/Index', ['posts' => $posts]);
}
```

In `app.js`:

```javascript
const pages = {
  // ... existing pages
  'Posts/Index': (props) => Layout(
    h('div', {},
      h('h1', {}, 'Posts'),
      // ... your component
    )
  ),
};
```

### Connect to a Database

Replace the mock `$users` array with real database queries:

```php
// Using PDO
$pdo = new PDO('mysql:host=localhost;dbname=mydb', 'user', 'pass');
$stmt = $pdo->query('SELECT * FROM users');
$users = $stmt->fetchAll(PDO::FETCH_ASSOC);
```

## Troubleshooting

**App not loading?**
- Make sure you've run `pnpm build` in the demo directory
- Check that `/dist/app.js` exists
- Check browser console for import errors

**Routes not working?**
- PHP built-in server handles all requests through index.php
- Make sure you're using `data-swbk` attribute on links for SPA navigation

**Docker issues?**
- Make sure port 8000 is not in use: `lsof -i :8000`
- Rebuild with `docker-compose build --no-cache`

**Build issues?**
- Make sure parent dependencies are installed: `pnpm install --dir ../../../`
- TypeScript errors? Check that tsconfig.json exists in project root
