#!/usr/bin/env -S deno run --allow-net --allow-read

/**
 * Deno + Switchback Recipe Manager - Type-Safe Server
 *
 * This server demonstrates full TypeScript type safety from server to client.
 * Notice how we import shared-types.ts - the SAME types used by the client!
 *
 * No build step needed. Deno runs TypeScript natively. 🦕
 */

import type {
  Recipe,
  RecipeBookPageProps,
  RecipeDetailPageProps,
  RecipeSearchPageProps,
  AddRecipePageProps,
} from './shared-types.ts';

const PORT = 8000;

// Sample recipe database (in-memory for demo)
// In production, this would be from Deno KV or a database
const recipes: Recipe[] = [
  {
    id: '1',
    title: 'Classic Margherita Pizza',
    description: 'Simple and delicious Italian pizza with fresh mozzarella, tomatoes, and basil.',
    author: 'Chef Mario',
    prepTime: 20,
    cookTime: 15,
    servings: 4,
    difficulty: 'medium',
    ingredients: [
      { name: 'pizza dough', amount: 1, unit: 'whole' },
      { name: 'san marzano tomatoes', amount: 1, unit: 'cup' },
      { name: 'fresh mozzarella', amount: 8, unit: 'oz' },
      { name: 'fresh basil leaves', amount: 10, unit: 'whole' },
      { name: 'olive oil', amount: 2, unit: 'tbsp' },
      { name: 'salt', amount: 1, unit: 'tsp' },
    ],
    steps: [
      'Preheat oven to 500°F (260°C) with pizza stone inside',
      'Roll out pizza dough on a floured surface',
      'Spread tomato sauce evenly, leaving a 1-inch border',
      'Tear mozzarella into pieces and distribute over sauce',
      'Drizzle with olive oil and sprinkle with salt',
      'Bake for 12-15 minutes until crust is golden and cheese is bubbly',
      'Top with fresh basil leaves and serve immediately',
    ],
    tags: ['italian', 'pizza', 'vegetarian', 'comfort-food'],
    nutrition: {
      calories: 285,
      protein: 12,
      carbs: 35,
      fat: 10,
    },
    imageUrl: 'https://images.unsplash.com/photo-1604068549290-dea0e4a305ca',
    createdAt: new Date('2024-01-15'),
  },
  {
    id: '2',
    title: 'Chocolate Chip Cookies',
    description: 'Chewy, gooey chocolate chip cookies that are impossible to resist.',
    author: 'Baker Betty',
    prepTime: 15,
    cookTime: 12,
    servings: 24,
    difficulty: 'easy',
    ingredients: [
      { name: 'all-purpose flour', amount: 2.25, unit: 'cup' },
      { name: 'butter', amount: 1, unit: 'cup' },
      { name: 'granulated sugar', amount: 0.75, unit: 'cup' },
      { name: 'brown sugar', amount: 0.75, unit: 'cup' },
      { name: 'eggs', amount: 2, unit: 'whole' },
      { name: 'vanilla extract', amount: 2, unit: 'tsp' },
      { name: 'baking soda', amount: 1, unit: 'tsp' },
      { name: 'salt', amount: 1, unit: 'tsp' },
      { name: 'chocolate chips', amount: 2, unit: 'cup' },
    ],
    steps: [
      'Preheat oven to 375°F (190°C)',
      'Cream together butter and sugars until fluffy',
      'Beat in eggs and vanilla extract',
      'In a separate bowl, combine flour, baking soda, and salt',
      'Gradually blend dry ingredients into creamed mixture',
      'Stir in chocolate chips',
      'Drop rounded tablespoons onto ungreased cookie sheets',
      'Bake for 9-11 minutes or until golden brown',
      'Cool on baking sheet for 2 minutes, then transfer to wire rack',
    ],
    tags: ['dessert', 'cookies', 'chocolate', 'baking'],
    nutrition: {
      calories: 180,
      protein: 2,
      carbs: 24,
      fat: 9,
    },
    imageUrl: 'https://images.unsplash.com/photo-1499636136210-6f4ee915583e',
    createdAt: new Date('2024-01-10'),
  },
  {
    id: '3',
    title: 'Thai Green Curry',
    description: 'Aromatic and spicy Thai curry with vegetables and coconut milk.',
    author: 'Chef Somchai',
    prepTime: 25,
    cookTime: 30,
    servings: 4,
    difficulty: 'hard',
    ingredients: [
      { name: 'green curry paste', amount: 3, unit: 'tbsp' },
      { name: 'coconut milk', amount: 2, unit: 'cup' },
      { name: 'chicken breast', amount: 1, unit: 'lb' },
      { name: 'bamboo shoots', amount: 1, unit: 'cup' },
      { name: 'thai basil', amount: 0.5, unit: 'cup' },
      { name: 'fish sauce', amount: 2, unit: 'tbsp' },
      { name: 'palm sugar', amount: 1, unit: 'tbsp' },
      { name: 'kaffir lime leaves', amount: 4, unit: 'whole' },
      { name: 'vegetable oil', amount: 2, unit: 'tbsp' },
    ],
    steps: [
      'Heat oil in a wok over medium-high heat',
      'Add green curry paste and fry for 2 minutes until fragrant',
      'Pour in half of the coconut milk and stir well',
      'Add chicken and cook until it changes color',
      'Add remaining coconut milk, bamboo shoots, and lime leaves',
      'Simmer for 15 minutes until chicken is cooked through',
      'Season with fish sauce and palm sugar',
      'Stir in thai basil just before serving',
      'Serve hot with jasmine rice',
    ],
    tags: ['thai', 'curry', 'spicy', 'asian'],
    nutrition: {
      calories: 420,
      protein: 28,
      carbs: 12,
      fat: 30,
    },
    imageUrl: 'https://images.unsplash.com/photo-1455619452474-d2be8b1e70cd',
    createdAt: new Date('2024-01-20'),
  },
  {
    id: '4',
    title: 'Caesar Salad',
    description: 'Classic Roman salad with crispy romaine, parmesan, and creamy dressing.',
    author: 'Chef Giovanni',
    prepTime: 15,
    cookTime: 10,
    servings: 4,
    difficulty: 'easy',
    ingredients: [
      { name: 'romaine lettuce', amount: 2, unit: 'whole' },
      { name: 'parmesan cheese', amount: 0.5, unit: 'cup' },
      { name: 'croutons', amount: 1, unit: 'cup' },
      { name: 'mayonnaise', amount: 0.5, unit: 'cup' },
      { name: 'garlic cloves', amount: 2, unit: 'whole' },
      { name: 'lemon juice', amount: 2, unit: 'tbsp' },
      { name: 'worcestershire sauce', amount: 1, unit: 'tsp' },
      { name: 'dijon mustard', amount: 1, unit: 'tsp' },
      { name: 'anchovy fillets', amount: 3, unit: 'whole' },
    ],
    steps: [
      'Wash and dry romaine lettuce, then tear into bite-sized pieces',
      'Make dressing: blend garlic, anchovies, lemon juice, mustard, and worcestershire',
      'Add mayonnaise and blend until smooth',
      'Toss lettuce with dressing in a large bowl',
      'Add croutons and toss again',
      'Top with shaved parmesan cheese',
      'Serve immediately',
    ],
    tags: ['salad', 'vegetarian', 'side-dish', 'italian'],
    nutrition: {
      calories: 210,
      protein: 8,
      carbs: 12,
      fat: 15,
    },
    imageUrl: 'https://images.unsplash.com/photo-1546793665-c74683f339c1',
    createdAt: new Date('2024-01-12'),
  },
];

// Helper function to get all unique tags
function getAllTags(): string[] {
  const tagsSet = new Set<string>();
  recipes.forEach(recipe => recipe.tags.forEach(tag => tagsSet.add(tag)));
  return Array.from(tagsSet).sort();
}

// Route handler - Returns fully typed page data
function handlePageRoute(url: URL): {
  component: string;
  props: RecipeBookPageProps | RecipeDetailPageProps | RecipeSearchPageProps | AddRecipePageProps;
  url: string;
} {
  const path = url.pathname;

  // Home page - Recipe book with all recipes
  if (path === '/' || path === '/recipes') {
    const props: RecipeBookPageProps = {
      recipes: recipes.slice().sort((a, b) =>
        b.createdAt.getTime() - a.createdAt.getTime()
      ),
      featuredRecipe: recipes[0], // Most recent
      popularTags: getAllTags().slice(0, 8),
    };

    return {
      component: 'RecipeBookPage',
      props,
      url: '/',
    };
  }

  // Recipe detail page
  const recipeMatch = path.match(/^\/recipe\/([^/]+)$/);
  if (recipeMatch) {
    const recipeId = recipeMatch[1];
    const recipe = recipes.find(r => r.id === recipeId);

    if (recipe) {
      // Find related recipes (same tags)
      const related = recipes
        .filter(r => r.id !== recipe.id)
        .filter(r => r.tags.some(tag => recipe.tags.includes(tag)))
        .slice(0, 3);

      const props: RecipeDetailPageProps = {
        recipe,
        relatedRecipes: related,
      };

      return {
        component: 'RecipeDetailPage',
        props,
        url: `/recipe/${recipeId}`,
      };
    }
  }

  // Search/filter page
  if (path === '/search') {
    const query = url.searchParams.get('q') || '';
    const tags = url.searchParams.getAll('tag');
    const difficulty = url.searchParams.get('difficulty');

    let filtered = recipes;

    // Filter by query
    if (query) {
      const lowerQuery = query.toLowerCase();
      filtered = filtered.filter(r =>
        r.title.toLowerCase().includes(lowerQuery) ||
        r.description.toLowerCase().includes(lowerQuery) ||
        r.tags.some(tag => tag.includes(lowerQuery))
      );
    }

    // Filter by tags
    if (tags.length > 0) {
      filtered = filtered.filter(r =>
        tags.some(tag => r.tags.includes(tag))
      );
    }

    // Filter by difficulty
    if (difficulty === 'easy' || difficulty === 'medium' || difficulty === 'hard') {
      filtered = filtered.filter(r => r.difficulty === difficulty);
    }

    const props: RecipeSearchPageProps = {
      recipes: filtered,
      query,
      selectedTags: tags,
      selectedDifficulty: (difficulty === 'easy' || difficulty === 'medium' || difficulty === 'hard')
        ? difficulty
        : undefined,
    };

    return {
      component: 'RecipeSearchPage',
      props,
      url: path,
    };
  }

  // Add recipe page
  if (path === '/add') {
    const props: AddRecipePageProps = {
      tags: getAllTags(),
    };

    return {
      component: 'AddRecipePage',
      props,
      url: '/add',
    };
  }

  // 404
  return {
    component: 'NotFound',
    props: {} as any, // NotFound doesn't need props
    url: path,
  };
}

// HTTP request handler
async function handleRequest(request: Request): Promise<Response> {
  const url = new URL(request.url);
  const path = url.pathname;
  const isSwitchback = request.headers.get('X-Switchback');

  console.log(`${request.method} ${path} ${isSwitchback ? '(Switchback)' : ''}`);

  // Serve bundled app.js
  if (path === '/dist/app.js') {
    try {
      const file = await Deno.readFile('./dist/app.js');
      return new Response(file, {
        headers: { 'Content-Type': 'application/javascript' },
      });
    } catch {
      return new Response('Build not found. Run: deno task build', { status: 404 });
    }
  }

  // Handle page routes
  const pageData = handlePageRoute(url);

  // Return JSON for Switchback requests
  if (isSwitchback) {
    return new Response(JSON.stringify(pageData), {
      headers: { 'Content-Type': 'application/json' },
    });
  }

  // Return full HTML for initial page load
  const html = `<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <title>Recipe Manager - Switchback + Deno</title>
  <style>
    body {
      margin: 0;
      padding: 0;
      font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
    }
  </style>
</head>
<body>
  <div id="app" data-swbk-app></div>
  <script>
    window.initialPage = ${JSON.stringify(pageData)};
  </script>
  <script type="module" src="/dist/app.js"></script>
</body>
</html>`;

  return new Response(html, {
    headers: { 'Content-Type': 'text/html' },
  });
}

// Start server
console.log(`🦕 Deno Recipe Manager starting on http://localhost:${PORT}`);
console.log('📚 Type-safe cookbook with Switchback integration');
console.log('');

Deno.serve({
  port: PORT,
  onListen: () => {
    console.log(`✅ Server running on http://localhost:${PORT}`);
    console.log('');
    console.log('Available routes:');
    console.log('  GET  /                 - Recipe book home');
    console.log('  GET  /recipe/:id       - Recipe detail');
    console.log('  GET  /search?q=...     - Search recipes');
    console.log('  GET  /add              - Add new recipe');
    console.log('');
    console.log('💡 Shared types: server.ts and app.ts use the same TypeScript interfaces!');
  },
}, handleRequest);

// Graceful shutdown
Deno.addSignalListener('SIGINT', () => {
  console.log('\n🛑 Shutting down gracefully...');
  Deno.exit(0);
});
