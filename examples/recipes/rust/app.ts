import { newSwitchback } from '../../../src/index.ts';

console.log('🦀 Product Catalog app starting...');

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
    } else {
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

// Global state
const state = {
  products: [] as any[],
  categories: [] as any[],
  currentProduct: null as any,
  loading: false,
};

// Inject styles
const style = document.createElement('style');
style.textContent = `
  * { margin: 0; padding: 0; box-sizing: border-box; }

  body {
    font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, Oxygen, Ubuntu, Cantarell, sans-serif;
    background: #f5f7fa;
    color: #333;
    line-height: 1.6;
    min-height: 100vh;
  }

  nav {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    padding: 1rem 2rem;
    box-shadow: 0 2px 10px rgba(0,0,0,0.1);
    position: sticky;
    top: 0;
    z-index: 100;
  }

  nav .nav-content {
    max-width: 1400px;
    margin: 0 auto;
    display: flex;
    justify-content: space-between;
    align-items: center;
    gap: 2rem;
  }

  nav h1 {
    font-size: 1.5rem;
    font-weight: 700;
  }

  nav .search-box {
    flex: 1;
    max-width: 400px;
  }

  nav input {
    width: 100%;
    padding: 0.5rem 1rem;
    border-radius: 20px;
    border: none;
    font-size: 0.95rem;
  }

  nav .nav-links {
    display: flex;
    gap: 1.5rem;
  }

  nav a {
    color: white;
    text-decoration: none;
    font-weight: 500;
    transition: opacity 0.2s;
  }

  nav a:hover {
    opacity: 0.8;
  }

  main {
    max-width: 1400px;
    margin: 0 auto;
    padding: 2rem;
  }

  .hero {
    background: white;
    border-radius: 12px;
    padding: 3rem 2rem;
    text-align: center;
    margin-bottom: 2rem;
    box-shadow: 0 2px 10px rgba(0,0,0,0.05);
  }

  .hero h2 {
    font-size: 2.5rem;
    color: #667eea;
    margin-bottom: 1rem;
  }

  .hero p {
    font-size: 1.1rem;
    color: #666;
    margin-bottom: 0.5rem;
  }

  .hero .tech-badge {
    display: inline-block;
    background: #f0f0f0;
    padding: 0.5rem 1rem;
    border-radius: 20px;
    margin: 0.5rem;
    font-size: 0.9rem;
    font-weight: 500;
  }

  .category-filters {
    display: flex;
    gap: 1rem;
    margin-bottom: 2rem;
    flex-wrap: wrap;
  }

  .category-btn {
    padding: 0.75rem 1.5rem;
    border-radius: 25px;
    border: 2px solid #e0e0e0;
    background: white;
    cursor: pointer;
    font-size: 0.95rem;
    font-weight: 500;
    transition: all 0.2s;
    box-shadow: 0 2px 4px rgba(0,0,0,0.05);
  }

  .category-btn:hover {
    border-color: #667eea;
    box-shadow: 0 4px 8px rgba(102, 126, 234, 0.2);
  }

  .category-btn.active {
    background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
    color: white;
    border-color: #667eea;
  }

  .product-grid {
    display: grid;
    grid-template-columns: repeat(auto-fill, minmax(280px, 1fr));
    gap: 2rem;
    margin-bottom: 2rem;
  }

  .product-card {
    background: white;
    border-radius: 12px;
    padding: 1.5rem;
    box-shadow: 0 2px 10px rgba(0,0,0,0.05);
    transition: transform 0.2s, box-shadow 0.2s;
    cursor: pointer;
    text-decoration: none;
    color: inherit;
    display: block;
  }

  .product-card:hover {
    transform: translateY(-5px);
    box-shadow: 0 10px 30px rgba(0,0,0,0.1);
  }

  .product-icon {
    width: 100%;
    height: 200px;
    object-fit: cover;
    border-radius: 8px;
    margin-bottom: 1rem;
  }

  .product-name {
    font-size: 1.2rem;
    font-weight: 600;
    margin-bottom: 0.5rem;
    color: #333;
  }

  .product-description {
    font-size: 0.9rem;
    color: #666;
    margin-bottom: 1rem;
    line-height: 1.4;
  }

  .product-category {
    display: inline-block;
    background: #f0f0f0;
    padding: 0.25rem 0.75rem;
    border-radius: 12px;
    font-size: 0.8rem;
    color: #667eea;
    margin-bottom: 1rem;
  }

  .product-footer {
    display: flex;
    justify-content: space-between;
    align-items: center;
    margin-top: 1rem;
    padding-top: 1rem;
    border-top: 1px solid #f0f0f0;
  }

  .product-price {
    font-size: 1.5rem;
    font-weight: 700;
    color: #667eea;
  }

  .product-stock {
    font-size: 0.85rem;
    color: #666;
  }

  .product-stock.low {
    color: #ff6b6b;
    font-weight: 500;
  }

  .product-detail {
    background: white;
    border-radius: 12px;
    padding: 3rem;
    box-shadow: 0 2px 10px rgba(0,0,0,0.05);
  }

  .product-detail-header {
    display: grid;
    grid-template-columns: 400px 1fr;
    gap: 3rem;
    margin-bottom: 2rem;
  }

  .product-detail-icon {
    width: 100%;
    height: 400px;
    object-fit: cover;
    border-radius: 12px;
  }

  .product-detail-info h2 {
    font-size: 2.5rem;
    color: #333;
    margin-bottom: 1rem;
  }

  .product-detail-info .price-large {
    font-size: 3rem;
    color: #667eea;
    font-weight: 700;
    margin-bottom: 1rem;
  }

  .back-button {
    display: inline-block;
    padding: 0.75rem 1.5rem;
    background: #667eea;
    color: white;
    text-decoration: none;
    border-radius: 8px;
    font-weight: 500;
    transition: all 0.2s;
    box-shadow: 0 2px 8px rgba(102, 126, 234, 0.3);
  }

  .back-button:hover {
    background: #5568d3;
    box-shadow: 0 4px 12px rgba(102, 126, 234, 0.4);
  }

  .loading-spinner {
    text-align: center;
    padding: 4rem 2rem;
    font-size: 3rem;
  }

  .info-box {
    background: #e8f4f8;
    border-left: 4px solid #667eea;
    padding: 1.5rem;
    border-radius: 8px;
    margin: 2rem 0;
  }

  .info-box h3 {
    color: #667eea;
    margin-bottom: 0.5rem;
  }

  .info-box ul {
    margin-left: 1.5rem;
    color: #555;
  }

  .info-box li {
    margin: 0.25rem 0;
  }
`;
document.head.appendChild(style);

// API helpers
async function fetchProducts(categoryId?: number) {
  const url = categoryId
    ? `/api/category/${categoryId}`
    : '/api/products';
  const response = await fetch(url);
  return response.json();
}

async function fetchProduct(id: number) {
  const response = await fetch(`/api/products/${id}`);
  return response.json();
}

async function fetchCategories() {
  const response = await fetch('/api/categories');
  return response.json();
}

async function searchProducts(query: string) {
  const response = await fetch(`/api/search?q=${encodeURIComponent(query)}`);
  return response.json();
}

// Layout component
function Layout(children: Node) {
  const nav = h('nav', {},
    h('div', { class: 'nav-content' },
      h('h1', {}, '🦀 Rust Product Catalog'),
      h('div', { class: 'search-box' },
        h('input', {
          type: 'text',
          placeholder: 'Search products...',
          onKeypress: (e: KeyboardEvent) => {
            if (e.key === 'Enter') {
              const query = (e.target as HTMLInputElement).value;
              if (query) {
                e.preventDefault();
                state.products = []; // Clear products to force refetch
                app.visit(`/search?q=${encodeURIComponent(query)}`);
              }
            }
          }
        })
      ),
      h('div', { class: 'nav-links' },
        h('a', { href: '/' }, 'All Products')
      )
    )
  );

  const main = h('main', {}, children);

  const container = h('div', {});
  container.appendChild(nav);
  container.appendChild(main);
  return container;
}

// Page Components
const pages: Record<string, (props: any) => Node> = {
  'Home': () => {
    // Fetch products and categories only if we don't have them
    if (state.products.length === 0 || state.categories.length === 0) {
      Promise.all([fetchProducts(), fetchCategories()]).then(([products, categories]) => {
        state.products = products;
        state.categories = categories;
        app.reload();
      });
    }

    return Layout(
      h('div', {},
        h('div', { class: 'hero' },
          h('h2', {}, '⚡ Switchback + Rust Backend'),
          h('p', {}, 'Client-side routing meets server-side power'),
          h('p', {}, 'Fast SPA navigation with type-safe Rust API endpoints'),
          h('div', {},
            h('span', { class: 'tech-badge' }, '⚡ Switchback SPA'),
            h('span', { class: 'tech-badge' }, '🦀 Rust API'),
            h('span', { class: 'tech-badge' }, '🔄 JSON Data Flow')
          )
        ),

        state.categories.length > 0 ? h('div', { class: 'category-filters' },
          h('button', {
            class: 'category-btn active',
            onClick: (e: Event) => {
              e.preventDefault();
              state.products = []; // Clear products to force refetch
              app.visit('/');
            }
          }, 'All Products'),
          ...state.categories.map((cat: any) =>
            h('button', {
              class: 'category-btn',
              onClick: (e: Event) => {
                e.preventDefault();
                state.products = []; // Clear products to force refetch
                app.visit(`/category/${cat.id}`);
              }
            }, cat.name)
          )
        ) : null,

        state.products.length > 0 ? h('div', { class: 'product-grid' },
          ...state.products.map((product: any) =>
            h('a', {
              href: `/product/${product.id}`,
              class: 'product-card',
              onClick: (e: Event) => {
                e.preventDefault();
                app.visit(`/product/${product.id}`);
              }
            },
              h('img', { class: 'product-icon', src: product.image_url, alt: product.name }),
              h('div', { class: 'product-category' }, product.category_name),
              h('div', { class: 'product-name' }, product.name),
              h('div', { class: 'product-description' }, product.description),
              h('div', { class: 'product-footer' },
                h('div', { class: 'product-price' }, `$${product.price.toFixed(2)}`),
                h('div', { class: product.stock < 50 ? 'product-stock low' : 'product-stock' },
                  `${product.stock} in stock`
                )
              )
            )
          )
        ) : h('div', { class: 'loading-spinner' }, '⏳'),

        h('div', { class: 'info-box' },
          h('h3', {}, '🔗 How Switchback + Rust Work Together'),
          h('p', {}, 'This demo showcases seamless frontend/backend interplay:'),
          h('ul', {},
            h('li', {}, '⚡ Switchback: app.visit() enables instant client-side navigation - zero page reloads'),
            h('li', {}, '🦀 Rust: Provides fast RESTful JSON API endpoints with compile-time type safety'),
            h('li', {}, '🔄 Data Flow: Switchback fetches from /api/* endpoints, Rust returns JSON, component re-renders'),
            h('li', {}, '🔍 Features: Category filtering, search, product details - all without page refreshes'),
            h('li', {}, '🚀 Result: SPA-level UX with backend flexibility - use any database, any hosting platform')
          ),
          h('p', { style: { marginTop: '1rem', fontWeight: '500' } }, 'Try clicking categories or products - notice the instant transitions!')
        )
      )
    );
  },

  'CategoryPage': (props: { categoryId: number }) => {
    // Only fetch if needed
    if (state.products.length === 0) {
      fetchProducts(props.categoryId).then((products) => {
        state.products = products;
        app.reload();
      });
    }

    if (state.categories.length === 0) {
      fetchCategories().then((categories) => {
        state.categories = categories;
        app.reload();
      });
    }

    const currentCategory = state.categories.find((c: any) => c.id === props.categoryId);

    return Layout(
      h('div', {},
        h('div', { class: 'hero' },
          h('h2', {}, currentCategory ? currentCategory.name : 'Category'),
          currentCategory ? h('p', {}, currentCategory.description) : null
        ),

        h('div', { class: 'category-filters' },
          h('button', {
            class: 'category-btn',
            onClick: (e: Event) => {
              e.preventDefault();
              state.products = [];
              app.visit('/');
            }
          }, 'All Products'),
          ...state.categories.map((cat: any) =>
            h('button', {
              class: cat.id === props.categoryId ? 'category-btn active' : 'category-btn',
              onClick: (e: Event) => {
                e.preventDefault();
                state.products = [];
                app.visit(`/category/${cat.id}`);
              }
            }, cat.name)
          )
        ),

        state.products.length > 0 ? h('div', { class: 'product-grid' },
          ...state.products.map((product: any) =>
            h('a', {
              href: `/product/${product.id}`,
              class: 'product-card',
              onClick: (e: Event) => {
                e.preventDefault();
                app.visit(`/product/${product.id}`);
              }
            },
              h('img', { class: 'product-icon', src: product.image_url, alt: product.name }),
              h('div', { class: 'product-name' }, product.name),
              h('div', { class: 'product-description' }, product.description),
              h('div', { class: 'product-footer' },
                h('div', { class: 'product-price' }, `$${product.price.toFixed(2)}`),
                h('div', { class: product.stock < 50 ? 'product-stock low' : 'product-stock' },
                  `${product.stock} in stock`
                )
              )
            )
          )
        ) : h('div', { class: 'loading-spinner' }, '⏳')
      )
    );
  },

  'ProductPage': (props: { productId: number }) => {
    // Only fetch if we don't have the product or it's a different product
    if (!state.currentProduct || state.currentProduct.id !== props.productId) {
      state.currentProduct = null; // Clear current product
      fetchProduct(props.productId).then((p) => {
        state.currentProduct = p;
        app.reload();
      });
    }

    return Layout(
      h('div', {},
        state.currentProduct ? h('div', { class: 'product-detail' },
          h('div', { class: 'product-detail-header' },
            h('img', { class: 'product-detail-icon', src: state.currentProduct.image_url, alt: state.currentProduct.name }),
            h('div', { class: 'product-detail-info' },
              h('div', { class: 'product-category' }, state.currentProduct.category_name),
              h('h2', {}, state.currentProduct.name),
              h('p', { style: { fontSize: '1.2rem', color: '#666', marginBottom: '2rem' } }, state.currentProduct.description),
              h('div', { class: 'price-large' }, `$${state.currentProduct.price.toFixed(2)}`),
              h('p', { class: state.currentProduct.stock < 50 ? 'product-stock low' : 'product-stock', style: { fontSize: '1.1rem' } },
                `${state.currentProduct.stock} units in stock`
              )
            )
          ),
          h('a', {
            href: '/',
            class: 'back-button',
            onClick: (e: Event) => {
              e.preventDefault();
              state.products = [];
              app.visit('/');
            }
          }, '← Back to Products')
        ) : h('div', { class: 'loading-spinner' }, '⏳')
      )
    );
  },

  'SearchPage': () => {
    const urlParams = new URLSearchParams(window.location.search);
    const query = urlParams.get('q') || '';

    // Only search if we don't have products or query changed
    if (query && state.products.length === 0) {
      searchProducts(query).then((products) => {
        state.products = products;
        app.reload();
      });
    }

    return Layout(
      h('div', {},
        h('div', { class: 'hero' },
          h('h2', {}, `🔍 Search Results for "${query}"`),
          h('p', {}, `Found ${state.products.length} products`)
        ),

        state.products.length > 0 ? h('div', { class: 'product-grid' },
          ...state.products.map((product: any) =>
            h('a', {
              href: `/product/${product.id}`,
              class: 'product-card',
              onClick: (e: Event) => {
                e.preventDefault();
                app.visit(`/product/${product.id}`);
              }
            },
              h('img', { class: 'product-icon', src: product.image_url, alt: product.name }),
              h('div', { class: 'product-category' }, product.category_name),
              h('div', { class: 'product-name' }, product.name),
              h('div', { class: 'product-description' }, product.description),
              h('div', { class: 'product-footer' },
                h('div', { class: 'product-price' }, `$${product.price.toFixed(2)}`),
                h('div', { class: product.stock < 50 ? 'product-stock low' : 'product-stock' },
                  `${product.stock} in stock`
                )
              )
            )
          )
        ) : query ? h('div', { style: { textAlign: 'center', padding: '3rem' } },
          h('p', { style: { fontSize: '1.2rem', color: '#666' } }, 'No products found')
        ) : null,

        h('a', {
          href: '/',
          class: 'back-button',
          style: { marginTop: '2rem' },
          onClick: (e: Event) => {
            e.preventDefault();
            state.products = [];
            app.visit('/');
          }
        }, '← Back to All Products')
      )
    );
  },
};

// Initialize Switchback
let app: any; // Declare globally so we can use it in components

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
    color: '#667eea',
    includeCSS: true,
    showSpinner: true,
  },
});

console.log('🦀 Product Catalog app initialized!');
