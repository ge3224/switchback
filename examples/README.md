# Switchback Examples

Interactive examples and recipes demonstrating Switchback's single-page app features.

## Quick Start

```bash
# From project root
pnpm install
pnpm dev
```

Then open http://localhost:5173/

## What's Included

### Main Demo (Client-Only)

The main example at http://localhost:5173/ demonstrates:

- **Link Interception**: Click any link - no full page reload!
- **Mock Backend**: Simulates server responses with JSON
- **Flash Messages**: Success/error notifications
- **Multiple Pages**: Home, Users list, User detail, About
- **Instant Navigation**: SPA speed with server-side routing patterns

### Backend Recipes

Real-world integration examples with different server stacks:

#### 📁 `recipes/php/` - Vanilla PHP

A minimal PHP example with no framework dependencies.

```bash
# Option 1: PHP built-in server
cd recipes/php
php -S localhost:8000

# Option 2: Docker
cd recipes/php
docker-compose up
```

Open http://localhost:8000

**What's demonstrated:**
- Simple routing with PHP's built-in server
- Detecting `X-Switchback` header
- Returning JSON vs HTML based on request type
- Docker setup for easy deployment

See [`recipes/php/README.md`](recipes/php/README.md) for details.

## How It Works

1. Switchback intercepts all `<a>` clicks
2. Fetches page data from server (with `X-Switchback` header)
3. Renders new page component
4. Updates browser history
5. All without a full page reload!

## Server-Side Integration Pattern

All recipes follow this pattern:

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

## Docker Notes

Recipes use multi-stage Docker builds to bundle Switchback with the client app. This creates intermediate build images that can accumulate over time.

**Clean up Docker build cache:**
```bash
# Remove dangling images and build cache
docker system prune

# More aggressive cleanup (removes all unused images)
docker system prune -a
```

**Check Docker disk usage:**
```bash
docker system df
```

## Contributing Recipes

Want to add a recipe for your favorite stack? Follow this structure:

```
recipes/
└── your-stack/
    ├── README.md           # Setup instructions
    ├── Dockerfile          # Docker image (multi-stage build)
    ├── docker-compose.yml  # Docker setup
    ├── app.ts              # Switchback client (TypeScript)
    ├── vite.config.ts      # Bundles app.ts + Switchback
    ├── package.json        # Build scripts
    ├── .gitignore          # Ignore dist/ and node_modules/
    └── [server files]      # Your backend
```
