/**
 * Deno + Switchback Recipe Manager - Type-Safe Client
 *
 * This client imports the SAME TypeScript types as the server!
 * Notice how props are fully typed - TypeScript validates everything.
 *
 * No build step for the server. Deno runs TypeScript natively. 🦕
 * Client is bundled with esbuild via `deno task build`.
 */

import { newSwitchback } from '../../../src/index.ts';
import type {
  Recipe,
  RecipeBookPageProps,
  RecipeDetailPageProps,
  RecipeSearchPageProps,
  AddRecipePageProps,
} from './shared-types.ts';
import {
  getTotalTime,
  formatTime,
  getDifficultyColor,
} from './shared-types.ts';

console.log('🦕 Recipe Manager starting...');

// Simple JSX-like helper
function h(tag: string, props: any = {}, ...children: any[]) {
  const element = document.createElement(tag);

  Object.keys(props || {}).forEach(key => {
    if (key.startsWith('on')) {
      element.addEventListener(key.slice(2).toLowerCase(), props[key]);
    } else if (key === 'class' || key === 'className') {
      element.className = props[key];
    } else if (key === 'style' && typeof props[key] === 'object') {
      Object.assign(element.style, props[key]);
    } else if (key !== 'ref') {
      element.setAttribute(key, props[key]);
    }
  });

  children.flat().filter(Boolean).forEach(child => {
    if (typeof child === 'string' || typeof child === 'number') {
      element.appendChild(document.createTextNode(String(child)));
    } else if (child instanceof Node) {
      element.appendChild(child);
    }
  });

  return element;
}

// Styles
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
    background: #f9fafb;
    color: #111827;
    line-height: 1.6;
  }

  nav {
    background: white;
    border-bottom: 1px solid #e5e7eb;
    padding: 1rem 2rem;
    position: sticky;
    top: 0;
    z-index: 100;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
  }

  nav .nav-content {
    max-width: 1200px;
    margin: 0 auto;
    display: flex;
    justify-content: space-between;
    align-items: center;
  }

  nav h1 {
    font-size: 1.5rem;
    font-weight: 700;
    color: #059669;
  }

  nav .nav-links {
    display: flex;
    gap: 1.5rem;
  }

  nav a {
    color: #6b7280;
    text-decoration: none;
    font-weight: 500;
    padding: 0.5rem 1rem;
    border-radius: 6px;
    transition: all 0.2s;
  }

  nav a:hover, nav a.active {
    background: #f3f4f6;
    color: #059669;
  }

  main {
    max-width: 1200px;
    margin: 0 auto;
    padding: 2rem;
  }

  .hero {
    background: linear-gradient(135deg, #059669 0%, #10b981 100%);
    color: white;
    padding: 3rem 2rem;
    border-radius: 12px;
    margin-bottom: 2rem;
    text-align: center;
  }

  .hero h2 {
    font-size: 2.5rem;
    margin-bottom: 0.5rem;
  }

  .hero p {
    font-size: 1.1rem;
    opacity: 0.9;
  }

  .recipe-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(300px, 1fr));
    gap: 1.5rem;
    margin-top: 2rem;
  }

  .recipe-card {
    background: white;
    border-radius: 12px;
    overflow: hidden;
    box-shadow: 0 1px 3px rgba(0,0,0,0.1);
    transition: all 0.2s;
    cursor: pointer;
  }

  .recipe-card:hover {
    transform: translateY(-4px);
    box-shadow: 0 4px 12px rgba(0,0,0,0.15);
  }

  .recipe-image {
    width: 100%;
    height: 200px;
    object-fit: cover;
    background: #e5e7eb;
  }

  .recipe-content {
    padding: 1.5rem;
  }

  .recipe-title {
    font-size: 1.25rem;
    font-weight: 700;
    margin-bottom: 0.5rem;
    color: #111827;
  }

  .recipe-description {
    color: #6b7280;
    font-size: 0.9rem;
    margin-bottom: 1rem;
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical;
    overflow: hidden;
  }

  .recipe-meta {
    display: flex;
    gap: 1rem;
    font-size: 0.85rem;
    color: #6b7280;
    margin-bottom: 1rem;
  }

  .recipe-meta span {
    display: flex;
    align-items: center;
    gap: 0.25rem;
  }

  .difficulty-badge {
    display: inline-block;
    padding: 0.25rem 0.75rem;
    border-radius: 20px;
    font-size: 0.75rem;
    font-weight: 600;
    text-transform: uppercase;
  }

  .tags {
    display: flex;
    flex-wrap: wrap;
    gap: 0.5rem;
    margin-top: 1rem;
  }

  .tag {
    background: #f3f4f6;
    padding: 0.25rem 0.75rem;
    border-radius: 16px;
    font-size: 0.75rem;
    color: #6b7280;
  }

  .recipe-detail {
    background: white;
    border-radius: 12px;
    padding: 2rem;
    margin-bottom: 2rem;
  }

  .recipe-header {
    margin-bottom: 2rem;
  }

  .recipe-header h2 {
    font-size: 2rem;
    margin-bottom: 0.5rem;
  }

  .ingredients-list {
    list-style: none;
    margin: 1rem 0;
  }

  .ingredients-list li {
    padding: 0.5rem 0;
    border-bottom: 1px solid #f3f4f6;
  }

  .steps-list {
    list-style: decimal;
    margin-left: 1.5rem;
  }

  .steps-list li {
    padding: 0.75rem 0;
    padding-left: 0.5rem;
  }

  .section-title {
    font-size: 1.5rem;
    font-weight: 700;
    margin: 2rem 0 1rem 0;
  }

  .nutrition-grid {
    display: grid;
    grid-template-columns: repeat(4, 1fr);
    gap: 1rem;
    margin: 1rem 0;
  }

  .nutrition-item {
    background: #f9fafb;
    padding: 1rem;
    border-radius: 8px;
    text-align: center;
  }

  .nutrition-value {
    font-size: 1.5rem;
    font-weight: 700;
    color: #059669;
  }

  .nutrition-label {
    font-size: 0.85rem;
    color: #6b7280;
    margin-top: 0.25rem;
  }
`;
document.head.appendChild(style);

// Layout component
function Layout(children: Node, currentPath: string = '/') {
  const nav = h('nav', {},
    h('div', { class: 'nav-content' },
      h('h1', {}, '🦕 Recipe Manager'),
      h('div', { class: 'nav-links' },
        h('a', {
          href: '/',
          class: currentPath === '/' ? 'active' : '',
          onClick: (e: Event) => {
            e.preventDefault();
            app.visit('/');
          }
        }, 'Home'),
        h('a', {
          href: '/search',
          class: currentPath === '/search' ? 'active' : '',
          onClick: (e: Event) => {
            e.preventDefault();
            app.visit('/search');
          }
        }, 'Search'),
      )
    )
  );

  const main = h('main', {}, children);

  const container = h('div', {});
  container.appendChild(nav);
  container.appendChild(main);
  return container;
}

// Recipe Card Component
function RecipeCard(recipe: Recipe) {
  return h('div', {
    class: 'recipe-card',
    onClick: () => app.visit(`/recipe/${recipe.id}`)
  },
    recipe.imageUrl ? h('img', {
      src: recipe.imageUrl,
      alt: recipe.title,
      class: 'recipe-image'
    }) : h('div', { class: 'recipe-image' }),
    h('div', { class: 'recipe-content' },
      h('div', { class: 'recipe-title' }, recipe.title),
      h('div', { class: 'recipe-description' }, recipe.description),
      h('div', { class: 'recipe-meta' },
        h('span', {}, '⏱️ ', formatTime(getTotalTime(recipe))),
        h('span', {}, '👥 ', `${recipe.servings} servings`),
      ),
      h('span', {
        class: 'difficulty-badge',
        style: {
          backgroundColor: getDifficultyColor(recipe.difficulty) + '20',
          color: getDifficultyColor(recipe.difficulty),
        }
      }, recipe.difficulty),
      h('div', { class: 'tags' },
        ...recipe.tags.slice(0, 3).map(tag =>
          h('span', { class: 'tag' }, tag)
        )
      )
    )
  );
}

// Page Components - Notice the fully typed props! ✨
const pages: Record<string, (props: any) => Node> = {
  'RecipeBookPage': (props: RecipeBookPageProps) => {
    // TypeScript knows exactly what props contains!
    // Try typing "props." in VS Code - you'll get autocomplete!

    return Layout(
      h('div', {},
        h('div', { class: 'hero' },
          h('h2', {}, '📚 Recipe Collection'),
          h('p', {}, `${props.recipes.length} delicious recipes to explore`)
        ),

        h('div', { class: 'section-title' }, 'Featured Recipe'),
        RecipeCard(props.featuredRecipe),

        h('div', { class: 'section-title' }, 'All Recipes'),
        h('div', { class: 'recipe-grid' },
          ...props.recipes.map(recipe => RecipeCard(recipe))
        )
      ),
      '/'
    );
  },

  'RecipeDetailPage': (props: RecipeDetailPageProps) => {
    const { recipe, relatedRecipes } = props;

    return Layout(
      h('div', {},
        h('div', { class: 'recipe-detail' },
          h('div', { class: 'recipe-header' },
            h('h2', {}, recipe.title),
            h('p', { style: { color: '#6b7280', fontSize: '1.1rem' } }, recipe.description),
            h('div', { class: 'recipe-meta', style: { marginTop: '1rem' } },
              h('span', {}, '👨‍🍳 ', recipe.author),
              h('span', {}, '⏱️ Prep: ', formatTime(recipe.prepTime)),
              h('span', {}, '🔥 Cook: ', formatTime(recipe.cookTime)),
              h('span', {}, '👥 ', `${recipe.servings} servings`),
              h('span', {
                class: 'difficulty-badge',
                style: {
                  backgroundColor: getDifficultyColor(recipe.difficulty) + '20',
                  color: getDifficultyColor(recipe.difficulty),
                }
              }, recipe.difficulty),
            ),
            h('div', { class: 'tags', style: { marginTop: '1rem' } },
              ...recipe.tags.map(tag =>
                h('span', { class: 'tag' }, tag)
              )
            )
          ),

          recipe.imageUrl ? h('img', {
            src: recipe.imageUrl,
            alt: recipe.title,
            style: {
              width: '100%',
              maxHeight: '400px',
              objectFit: 'cover',
              borderRadius: '8px',
              margin: '1rem 0'
            }
          }) : null,

          h('h3', { class: 'section-title' }, '🥘 Ingredients'),
          h('ul', { class: 'ingredients-list' },
            ...recipe.ingredients.map(ing =>
              h('li', {}, `${ing.amount} ${ing.unit} ${ing.name}`)
            )
          ),

          h('h3', { class: 'section-title' }, '👩‍🍳 Instructions'),
          h('ol', { class: 'steps-list' },
            ...recipe.steps.map(step =>
              h('li', {}, step)
            )
          ),

          h('h3', { class: 'section-title' }, '📊 Nutrition (per serving)'),
          h('div', { class: 'nutrition-grid' },
            h('div', { class: 'nutrition-item' },
              h('div', { class: 'nutrition-value' }, recipe.nutrition.calories),
              h('div', { class: 'nutrition-label' }, 'Calories')
            ),
            h('div', { class: 'nutrition-item' },
              h('div', { class: 'nutrition-value' }, `${recipe.nutrition.protein}g`),
              h('div', { class: 'nutrition-label' }, 'Protein')
            ),
            h('div', { class: 'nutrition-item' },
              h('div', { class: 'nutrition-value' }, `${recipe.nutrition.carbs}g`),
              h('div', { class: 'nutrition-label' }, 'Carbs')
            ),
            h('div', { class: 'nutrition-item' },
              h('div', { class: 'nutrition-value' }, `${recipe.nutrition.fat}g`),
              h('div', { class: 'nutrition-label' }, 'Fat')
            )
          )
        ),

        relatedRecipes.length > 0 ? h('div', {},
          h('h3', { class: 'section-title' }, 'Related Recipes'),
          h('div', { class: 'recipe-grid' },
            ...relatedRecipes.map(recipe => RecipeCard(recipe))
          )
        ) : null
      ),
      `/recipe/${recipe.id}`
    );
  },

  'RecipeSearchPage': (props: RecipeSearchPageProps) => {
    return Layout(
      h('div', {},
        h('div', { class: 'hero' },
          h('h2', {}, '🔍 Search Results'),
          props.query ? h('p', {}, `Found ${props.recipes.length} recipes for "${props.query}"`) :
          h('p', {}, `Showing ${props.recipes.length} recipes`)
        ),

        props.recipes.length > 0 ?
          h('div', { class: 'recipe-grid' },
            ...props.recipes.map(recipe => RecipeCard(recipe))
          ) :
          h('p', { style: { textAlign: 'center', color: '#6b7280', marginTop: '2rem' } },
            'No recipes found. Try a different search term!'
          )
      ),
      '/search'
    );
  },

  'NotFound': () => {
    return Layout(
      h('div', { class: 'hero' },
        h('h2', {}, '404 Not Found'),
        h('p', {}, 'The page you are looking for does not exist.'),
        h('a', {
          href: '/',
          style: {
            display: 'inline-block',
            marginTop: '1rem',
            padding: '0.75rem 1.5rem',
            background: 'white',
            color: '#059669',
            borderRadius: '6px',
            textDecoration: 'none',
            fontWeight: '600'
          },
          onClick: (e: Event) => {
            e.preventDefault();
            app.visit('/');
          }
        }, 'Go Home')
      ),
      '/404'
    );
  }
};

// Initialize Switchback
let app: any;

app = newSwitchback({
  resolve: (name: string) => {
    const component = pages[name];
    if (!component) {
      throw new Error(`Component "${name}" not found`);
    }
    return component;
  },

  setup: ({ el, App, props }) => {
    el.innerHTML = '';
    el.appendChild(App(props));
  },

  initialPage: (window as any).initialPage,

  progress: {
    delay: 250,
    color: '#059669',
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('🦕 Recipe Manager initialized!');
console.log('💡 All props are fully typed thanks to shared-types.ts');
