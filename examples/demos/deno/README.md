# Deno Recipe - Switchback Integration

A full-featured Recipe Manager demonstrating **Shared TypeScript Types** between server and client. Shows Deno's killer feature: native TypeScript with zero build step for the server!

## What's Included

- **shared-types.ts** - TypeScript types used by BOTH server and client
- **server.ts** - Type-safe Deno HTTP server with recipe data
- **app.ts** - Client-side Switchback app with fully typed components
- **deno.json** - Deno configuration with build tasks
- **Docker setup** - Multi-stage build for production deployment

## The Key Feature: Shared TypeScript Types

This demo showcases Deno's unique advantage - **the same TypeScript types** are imported by both server and client:

```typescript
// shared-types.ts - imported by BOTH server.ts AND app.ts
export interface Recipe {
  id: string;
  title: string;
  description: string;
  author: string;
  prepTime: number;
  cookTime: number;
  servings: number;
  difficulty: 'easy' | 'medium' | 'hard';
  ingredients: Ingredient[];
  steps: string[];
  tags: string[];
  nutrition: NutritionInfo;
  imageUrl?: string;
  createdAt: Date;
}
```

### Server-Side (server.ts)
```typescript
import type { Recipe, RecipeBookPageProps } from './shared-types.ts';

const recipes: Recipe[] = [
  { id: '1', title: 'Margherita Pizza', /* ... fully typed! */ },
];

function handlePageRoute(url: URL): {
  component: string;
  props: RecipeBookPageProps | RecipeDetailPageProps;
  url: string;
} {
  // TypeScript validates everything at compile time!
  const props: RecipeBookPageProps = {
    recipes: recipes,
    featuredRecipe: recipes[0],
    popularTags: getAllTags(),
  };
  return { component: 'RecipeBookPage', props, url: '/' };
}
```

### Client-Side (app.ts)
```typescript
import type { RecipeBookPageProps } from './shared-types.ts';
import { getTotalTime, formatTime } from './shared-types.ts';

const pages = {
  'RecipeBookPage': (props: RecipeBookPageProps) => {
    // TypeScript knows EXACTLY what's in props!
    // Try typing "props." - you get autocomplete for:
    // - props.recipes (Recipe[])
    // - props.featuredRecipe (Recipe)
    // - props.popularTags (string[])

    return h('div', {},
      h('p', {}, `${props.recipes.length} delicious recipes`),
      h('div', { class: 'recipe-grid' },
        ...props.recipes.map(recipe => RecipeCard(recipe))
      )
    );
  },
};
```

### Shared Helper Functions

Even better - utility functions can be shared too:

```typescript
// shared-types.ts
export function getTotalTime(recipe: Recipe): number {
  return recipe.prepTime + recipe.cookTime;
}

export function formatTime(minutes: number): string {
  if (minutes < 60) return `${minutes} min`;
  const hours = Math.floor(minutes / 60);
  const mins = minutes % 60;
  return mins > 0 ? `${hours}h ${mins}m` : `${hours}h`;
}
```

These work on both server and client!

## Running Locally

### Prerequisites

- Deno 2.0+ ([install guide](https://deno.land/manual/getting_started/installation))
- No Node.js needed! (But esbuild is used for client bundling)

### Build and Run

```bash
cd examples/demos/deno

# Build the client bundle (bundles app.ts + Switchback)
deno task build

# Run the server (no build step needed - Deno runs TypeScript!)
deno task serve
```

Open http://localhost:8000

**For development with auto-rebuild:**

```bash
# Terminal 1: Watch and rebuild client on changes
deno task build --watch

# Terminal 2: Watch and restart server on changes
deno task dev
```

### Option 2: Docker (Recommended)

Docker will automatically build everything:

```bash
# From examples/demos/deno directory
docker-compose up

# Or build first, then run
docker-compose build
docker-compose up
```

Open http://localhost:8000

**Note:** The Dockerfile uses a multi-stage build:
1. **Deno builder stage**: Bundles client TypeScript with esbuild
2. **Runtime stage**: Minimal Deno image with compiled bundle
3. Server runs TypeScript directly - no transpilation!

## Available Routes

### Recipe Book
- `GET /` - Home page with all recipes and featured recipe
- Shows recipe cards with images, descriptions, difficulty badges
- Click any recipe to view details

### Recipe Detail
- `GET /recipe/:id` - Full recipe with ingredients, steps, nutrition
- Related recipes based on shared tags
- Fully typed with `RecipeDetailPageProps`

### Search
- `GET /search?q=pizza` - Search by keyword
- `GET /search?tag=italian` - Filter by tag
- `GET /search?difficulty=easy` - Filter by difficulty
- Combine filters: `?q=pizza&tag=italian&difficulty=medium`

## Project Structure

```
deno/
├── shared-types.ts      # Types used by both server and client
├── server.ts            # Deno HTTP server with typed routes
├── app.ts               # Client Switchback app with typed components
├── deno.json            # Deno config + build tasks
├── Dockerfile           # Docker image
├── docker-compose.yml   # Docker setup
└── README.md            # This file
```

## Key Features Demonstrated

- ✅ **Shared TypeScript Types** - Same interfaces on server and client
- ✅ **Zero-config TypeScript** - Deno runs .ts files natively
- ✅ **Full type safety** - Compile-time validation across full stack
- ✅ **Shared utility functions** - Helper functions work everywhere
- ✅ **Type-safe Switchback** - Page props are fully typed
- ✅ **Recipe domain model** - Complex nested types (Recipe, Ingredient, Nutrition)
- ✅ **Search and filtering** - Query params with type validation
- ✅ **Related recipes** - Tag-based recommendation system

## Why This Matters

Traditional Node.js/Express approach:

```typescript
// server.ts (Node + Express)
interface Recipe { /* types */ }
const recipes: Recipe[] = [/* data */];

app.get('/api/recipes', (req, res) => {
  res.json(recipes); // Type safety ends here
});

// client.ts (separate build)
// Have to duplicate Recipe type OR use codegen
interface Recipe { /* duplicate types! */ }
fetch('/api/recipes').then(data => {
  // No compile-time guarantee data matches Recipe
});
```

**Deno + Switchback approach:**

```typescript
// shared-types.ts
export interface Recipe { /* types */ }

// server.ts - imports shared types
import type { Recipe } from './shared-types.ts';
const recipes: Recipe[] = [/* data */];

// app.ts - imports THE SAME types
import type { Recipe } from './shared-types.ts';
// TypeScript validates everything!
```

**Benefits:**
- 🎯 Single source of truth for types
- ⚡ No type generation or duplication
- 🔒 Compile-time safety across the stack
- 🛠️ Refactor once, type-check everywhere
- 📦 Helper functions shared between server/client

## Comparison with Other Demos

- **PHP Demo**: Shows basic navigation with a scripting language
- **C Demo**: Shows optimistic updates with systems programming
- **Rust Demo**: Shows database integration with compile-time safety
- **Zig Demo**: Shows form handling with modern systems language
- **Deno Demo**: Shows **TypeScript type sharing** across client/server

Each demo demonstrates different Switchback patterns and language ecosystems!

## Extending This Example

### Add a New Field to Recipe

In `shared-types.ts`:

```typescript
export interface Recipe {
  id: string;
  title: string;
  // ... existing fields
  cuisine: string; // NEW: cuisine type
}
```

That's it! TypeScript will now validate this field everywhere:
- Server data must include `cuisine`
- Client components can access `recipe.cuisine` safely
- Autocomplete works in both server.ts and app.ts

### Add Recipe Creation

In `server.ts`:

```typescript
if (request.method === 'POST' && path === '/api/recipes') {
  const body = await request.json();

  // TypeScript validates the structure!
  const newRecipe: Recipe = {
    id: String(recipes.length + 1),
    title: body.title,
    description: body.description,
    // ... TypeScript ensures all required fields
  };

  recipes.push(newRecipe);

  return Response.json({ recipe: newRecipe });
}
```

In `app.ts`, add an "Add Recipe" page with a form that submits to `/api/recipes`.

### Add Deno KV for Persistence

Replace in-memory storage with Deno KV:

```typescript
const kv = await Deno.openKv();

// Save recipe
await kv.set(['recipes', recipe.id], recipe);

// Load all recipes
const entries = kv.list<Recipe>({ prefix: ['recipes'] });
const recipes: Recipe[] = [];
for await (const entry of entries) {
  recipes.push(entry.value);
}
```

Deno KV is built-in - no external database needed!

### Add Authentication

Use Deno's built-in Web Crypto API:

```typescript
import { create, verify } from 'https://deno.land/x/djwt/mod.ts';

const jwt = await create({ alg: 'HS512', typ: 'JWT' }, { userId: '123' }, key);
const payload = await verify(jwt, key);
```

## Troubleshooting

**App not loading?**
- Make sure you've run `deno task build`
- Check that `dist/app.js` exists
- Check browser console for import errors

**Deno errors?**
- Update Deno: `deno upgrade`
- Check version: `deno --version` (need 2.0+)
- Clear cache: `deno cache --reload server.ts`

**Type errors in VS Code?**
- Install Deno extension: https://marketplace.visualstudio.com/items?itemName=denoland.vscode-deno
- Enable Deno in workspace: Cmd+Shift+P → "Deno: Initialize Workspace Configuration"
- Restart VS Code

**Docker issues?**
- Make sure port 8000 is not in use: `lsof -i :8000`
- Rebuild with `docker-compose build --no-cache`
- Check logs: `docker-compose logs`

**Build issues?**
- esbuild not found? Run `deno task build` from demo directory
- Module not found? Check import paths use `.ts` extension
- Permission denied? Deno requires explicit permissions: `--allow-net --allow-read`

## Performance Notes

Deno's TypeScript performance:
- **Server startup**: ~50ms (TypeScript runs natively!)
- **Type checking**: Done at startup, zero runtime overhead
- **Hot reload**: Fast incremental compilation
- **Memory usage**: ~30MB for server (efficient V8 engine)
- **HTTP throughput**: Comparable to Node.js (~50k req/sec)

The client bundle is built once with esbuild (~100ms build time).

## Security Considerations

This is a **demo application**. For production, add:

- ✅ Input validation (use Zod or similar)
- ✅ CSRF protection for POST requests
- ✅ Rate limiting (use `x/ratelimit` middleware)
- ✅ Authentication with JWT or sessions
- ✅ HTTPS with TLS certificates
- ✅ Content Security Policy headers
- ✅ Sanitize user-generated content

Deno's security model is permission-based - explicitly allow what you need:
```bash
deno run --allow-net=:8000 --allow-read=./dist server.ts
```

## Why Deno?

This demo showcases Deno's strengths:

- **TypeScript-first**: No tsconfig, no babel, no webpack for server
- **Modern APIs**: Web standards (fetch, Response, URL) built-in
- **Simple tooling**: `deno task` replaces npm scripts
- **Secure by default**: Explicit permissions required
- **Batteries included**: HTTP server, testing, formatting, linting all built-in
- **Type sharing**: Import .ts files directly - share types everywhere!

Perfect for building type-safe full-stack applications with minimal configuration!

## Learn More About Type Safety

- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
- [Deno Manual](https://deno.land/manual)
- [Type-safe APIs](https://www.apollographql.com/blog/backend/architecture/typescript-api-type-safety/)
- [End-to-end Type Safety](https://www.prisma.io/blog/full-stack-typesafety-with-react-graphql-prisma)

Type safety across the full stack prevents bugs and improves developer experience!
