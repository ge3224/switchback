# Rust + Switchback Product Catalog

A full-stack demo showcasing **Switchback's seamless integration with a Rust backend**.

## 🎯 What This Demonstrates

This demo shows the **powerful interplay between Switchback and Rust**:

- **Switchback (Frontend)**: Client-side routing with `app.visit()`, state management, instant navigation
- **Rust (Backend)**: Type-safe RESTful JSON API with compile-time guarantees
- **Integration**: Seamless data flow between SPA frontend and native backend
- **Portability**: Uses SQLite for zero-config setup (easily swap for Postgres, MongoDB, etc.)

## 🏗️ Architecture

```
┌─────────────────────────────────────────────────┐
│  Switchback (Frontend)                          │
│  ├─ app.visit() for client-side routing         │
│  ├─ State management (no Redux needed)          │
│  ├─ fetch() calls to /api/* endpoints           │
│  └─ Component re-rendering on state changes     │
└─────────────────┬───────────────────────────────┘
                  │ HTTP/JSON
┌─────────────────▼───────────────────────────────┐
│  Rust Backend (API Server)                      │
│  ├─ RESTful JSON endpoints                      │
│  ├─ Compile-time type safety (serde)            │
│  ├─ Thread-safe concurrency (Arc<Mutex>)        │
│  └─ Database queries (SQLite for portability)   │
└─────────────────────────────────────────────────┘
```

## 🔗 Frontend/Backend Interplay

### Switchback Routes → Rust API Endpoints

| Switchback Route | Component | Rust API Endpoint | What Rust Returns |
|------------------|-----------|-------------------|-------------------|
| `/` | Home | `GET /api/products` | All products (JSON array) |
| `/category/:id` | CategoryPage | `GET /api/category/:id` | Filtered products (JSON) |
| `/product/:id` | ProductPage | `GET /api/products/:id` | Single product (JSON) |
| `/search?q=...` | SearchPage | `GET /api/search?q=...` | Search results (JSON) |

### How Navigation Works

1. **User clicks a category button**
   - `onClick` handler calls `app.visit('/category/2')`
   - Switchback intercepts, shows progress bar
   - Fetches `/category/2` with `X-Switchback` header
   - Server returns JSON: `{"component": "CategoryPage", "props": {"categoryId": 2}}`
   - Component mounts, calls `fetchProducts(2)` → `GET /api/category/2`
   - Rust queries SQLite with `WHERE category_id = 2`
   - Returns JSON array of products
   - Switchback re-renders with new data
   - **No page reload!**

2. **User clicks a product card**
   - `app.visit('/product/5')`
   - Component calls `fetchProduct(5)` → `GET /api/products/5`
   - Rust performs SQL JOIN and returns product with category name
   - Smooth transition to detail view

3. **User searches**
   - `app.visit('/search?q=laptop')`
   - Component calls `searchProducts('laptop')` → `GET /api/search?q=laptop`
   - Rust executes `WHERE name LIKE '%laptop%'`
   - Results displayed instantly

## 🦀 Why Rust?

### Advantages Over Node.js/Next.js

| Feature | Rust + SQLite | Node.js/Next.js |
|---------|---------------|-----------------|
| **Deployment** | Single binary | Node runtime + app + database |
| **Database** | Embedded in binary | Requires PostgreSQL/MySQL/MongoDB |
| **Setup** | Zero config | Connection strings, migrations, env vars |
| **Performance** | Native speed | V8 overhead |
| **Memory** | Minimal footprint | Higher memory usage |
| **Type Safety** | Compile-time SQL checks | Runtime errors |
| **Concurrency** | Fearless (Arc<Mutex>) | Event loop complexity |

### Perfect For

- ✅ **Edge computing** - Single binary, minimal resources
- ✅ **Embedded systems** - IoT devices, kiosks
- ✅ **Simple deployment** - Copy one file, run
- ✅ **Offline-first** - Local database, no network dependency
- ✅ **High performance** - Native code, zero-cost abstractions

## 🚀 Running the Demo

```bash
# Install dependencies
pnpm install

# Build JavaScript bundle
pnpm build:js

# Run the Rust server
cargo run --features server

# Visit http://localhost:8000
```

The database will be created automatically on first run with sample data.

## 📁 Project Structure

```
rust/
├── server.rs          # Rust HTTP server + SQLite
├── app.ts             # Switchback frontend
├── Cargo.toml         # Rust dependencies
├── package.json       # Node dependencies
└── products.db        # SQLite database (auto-created)
```

## 🔧 Key Technologies

- **Switchback**: Vanilla JavaScript SPA framework with HTML-over-the-wire
- **Rust**: Systems programming language with memory safety
- **rusqlite**: SQLite bindings for Rust with `bundled` feature (no system deps)
- **serde**: Serialization framework for Rust → JSON
- **Arc<Mutex<Connection>>**: Thread-safe database access

## 💡 Code Walkthrough

### Backend: Rust Server (server.rs)

```rust
// Database initialization with embedded SQLite
fn init_database() -> Result<Connection> {
    let conn = Connection::open("products.db")?;

    // Create tables
    conn.execute("CREATE TABLE IF NOT EXISTS categories ...", [])?;
    conn.execute("CREATE TABLE IF NOT EXISTS products ...", [])?;

    // Seed sample data if empty
    if count == 0 {
        seed_database(&conn)?;
    }

    Ok(conn)
}

// API endpoint: Get products by category
if uri.starts_with("/api/category/") {
    let cat_id = /* parse from URI */;
    match get_all_products(&conn, Some(cat_id)) {
        Ok(products) => {
            let json = serde_json::to_string(&products).unwrap();
            send_json_response(&mut stream, &json);
        }
    }
}

// SQL query with JOIN
fn get_all_products(conn: &Connection, category_id: Option<i64>) -> Result<Vec<Product>> {
    let query = "SELECT p.*, c.name as category_name
                 FROM products p
                 JOIN categories c ON p.category_id = c.id
                 WHERE ...";
    // rusqlite handles type safety at compile time
}
```

### Frontend: Switchback App (app.ts)

```typescript
// Client-side navigation (no page reloads!)
h('button', {
  onClick: (e: Event) => {
    e.preventDefault();
    state.products = []; // Clear state to force refetch
    app.visit(`/category/${cat.id}`); // Switchback navigation
  }
}, 'Electronics')

// Component fetches data from API
'CategoryPage': (props: { categoryId: number }) => {
  if (state.products.length === 0) {
    fetchProducts(props.categoryId).then((products) => {
      state.products = products;
      app.reload(); // Re-render with new data
    });
  }

  return Layout(/* render product grid */);
}

// Fetch function calls Rust API
async function fetchProducts(categoryId?: number) {
  const url = categoryId
    ? `/api/category/${categoryId}`
    : '/api/products';
  const response = await fetch(url);
  return response.json(); // Rust returns JSON
}
```

## 🎓 Key Concepts

### 1. Embedded Database

```rust
// No PostgreSQL installation needed!
let conn = Connection::open("products.db")?;

// Database file created automatically
// Bundled SQLite compiled into binary
// Zero configuration required
```

**vs Node.js:**
```javascript
// Requires external database
const client = new Client({
  host: process.env.DB_HOST,
  port: process.env.DB_PORT,
  database: process.env.DB_NAME,
  user: process.env.DB_USER,
  password: process.env.DB_PASSWORD,
});
```

### 2. Type-Safe Queries

```rust
#[derive(Serialize, Deserialize)]
struct Product {
    id: i64,
    name: String,
    price: f64,
    category_id: i64,
}

// Compiler ensures types match SQL schema
conn.query_row("SELECT id, name, price FROM products WHERE id = ?1",
    params![id],
    |row| {
        Ok(Product {
            id: row.get(0)?,  // Compile-time type check!
            name: row.get(1)?,
            price: row.get(2)?,
        })
    }
)
```

### 3. Thread-Safe Concurrency

```rust
// Arc<Mutex<>> ensures safe concurrent access
let db = Arc::new(Mutex::new(conn));

// Each request thread can access database safely
thread::spawn(move || {
    let conn = db.lock().unwrap();
    // Query database
});
```

### 4. Client-Side Navigation

```typescript
// Traditional approach (full page reload)
window.location.href = '/category/2'; // ❌ Slow!

// Switchback approach (SPA navigation)
app.visit('/category/2'); // ✅ Fast!
// - No page refresh
// - Shows progress bar
// - Fetches only new data
// - Updates URL
```

## 📊 Data Flow Example

**User clicks "Electronics" category:**

```
1. Browser:    onClick → app.visit('/category/1')
               ↓
2. Switchback: Intercept navigation, show progress bar
               ↓
3. Browser:    GET /category/1 (with X-Switchback header)
               ↓
4. Rust:       handle_page_route() → return CategoryPage JSON
               ↓
5. Switchback: Mount CategoryPage component
               ↓
6. Component:  fetchProducts(1)
               ↓
7. Browser:    GET /api/category/1
               ↓
8. Rust:       get_all_products(conn, Some(1))
               ↓
9. SQLite:     SELECT * FROM products WHERE category_id = 1
               ↓
10. Rust:      serde_json::to_string(&products)
               ↓
11. Browser:   Parse JSON, update state
               ↓
12. Switchback: app.reload()
               ↓
13. Browser:   Render product grid (no page reload!)
```

## 🎯 Learning Points

1. **Switchback + Rust = Best of Both Worlds**
   - Switchback handles instant SPA navigation (`app.visit()`)
   - Rust provides blazing-fast API endpoints with type safety
   - Simple integration: Switchback fetches JSON, Rust returns JSON

2. **No framework lock-in**
   - Switchback works with any backend (Rust, Go, Python, PHP, etc.)
   - Rust works with any frontend (React, Vue, vanilla JS, etc.)
   - This demo shows they work beautifully together

3. **Practical architecture**
   - Frontend: Client-side routing, state management, progressive enhancement
   - Backend: RESTful API, compile-time safety, thread-safe concurrency
   - Database: SQLite for portability (swap for Postgres/MongoDB as needed)

## 🚀 Next Steps

- Add **write operations** (POST/PUT/DELETE) for full CRUD
- Implement **pagination** for large product lists
- Add **authentication** with JWT tokens
- Try **other Rust web frameworks** (axum, actix-web)
- **Optimize queries** with indexes and prepared statements
- **Deploy to Fly.io** or AWS Lambda (single binary = easy deploy!)

## 📚 Related Resources

- [Switchback Documentation](https://github.com/switchback-org/switchback)
- [rusqlite Documentation](https://docs.rs/rusqlite/)
- [Rust Book](https://doc.rust-lang.org/book/)
- [SQLite Documentation](https://www.sqlite.org/docs.html)

## 🔍 Comparison with Other Demos

- **PHP Demo**: Traditional LAMP stack with server-side rendering
- **Go Demo**: Concurrent worker pools with goroutines
- **Zig Demo**: Manual memory management for systems programming
- **C Demo**: Low-level HTTP server with optimistic updates
- **Rust Demo**: Embedded database with type safety and zero config

Each demonstrates different Switchback integrations. **Rust's embedded SQLite support is unique** - perfect for applications that need a database without external dependencies.

---

**Built with ❤️ to showcase Rust + Switchback integration**
