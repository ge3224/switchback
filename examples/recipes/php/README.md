# PHP Recipe - Switchback Integration

A minimal example showing how to integrate Switchback with vanilla PHP. No frameworks needed!

## What's Included

- **index.php** - Simple PHP router that detects `X-Switchback` header and returns JSON
- **app.ts** - Client-side Switchback app (TypeScript)
- **vite.config.ts** - Bundles app.ts + Switchback into single JS file
- **Docker setup** - One-command startup with PHP built-in server

## Running Locally

### Option 1: PHP Built-in Server (Recommended)

Build the bundled client app:

```bash
cd examples/recipes/php

# Install dependencies (uses parent's node_modules for vite/typescript)
pnpm install --dir ../../../

# Build app.ts + Switchback into dist/app.js
pnpm build
```

Then start the PHP server:

```bash
php -S localhost:8000
```

Open http://localhost:8000

**For development with auto-rebuild:**

```bash
# Terminal 1: Watch and rebuild on changes
pnpm dev

# Terminal 2: Run PHP server
php -S localhost:8000
```

### Option 2: Docker

Docker will automatically build everything:

```bash
# From examples/recipes/php directory
docker-compose up

# Or build first, then run
docker-compose build
docker-compose up
```

Open http://localhost:8000

**Note:** The Dockerfile uses a multi-stage build:
1. **Builder stage**: Bundles app.ts with Switchback source into single JS file
2. **Runtime stage**: PHP 8.3 serving the bundled app

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
- Make sure you've run `pnpm build` in the recipe directory
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
