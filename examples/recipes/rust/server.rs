/**
 * Rust Recipe - Switchback HTTP Server
 * Product Catalog with Embedded SQLite Database
 * Demonstrates Rust's advantage: single-binary deployment with zero external dependencies
 */

use rusqlite::{params, Connection, Result};
use serde::{Deserialize, Serialize};
use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::sync::{Arc, Mutex};
use std::sync::atomic::{AtomicBool, Ordering};
use std::thread;
use std::time::Duration;
use std::fs;

const PORT: u16 = 8000;
const BUFFER_SIZE: usize = 16384; // Increased for JSON payloads

// Data models
#[derive(Debug, Serialize, Deserialize)]
struct Category {
    id: i64,
    name: String,
    description: String,
}

#[derive(Debug, Serialize, Deserialize)]
struct Product {
    id: i64,
    name: String,
    description: String,
    price: f64,
    category_id: i64,
    category_name: String,
    stock: i32,
    image_url: String,
}

// Database functions
fn init_database() -> Result<Connection> {
    let conn = Connection::open("products.db")?;

    // Create categories table
    conn.execute(
        "CREATE TABLE IF NOT EXISTS categories (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL,
            description TEXT NOT NULL
        )",
        [],
    )?;

    // Create products table
    conn.execute(
        "CREATE TABLE IF NOT EXISTS products (
            id INTEGER PRIMARY KEY,
            name TEXT NOT NULL,
            description TEXT NOT NULL,
            price REAL NOT NULL,
            category_id INTEGER NOT NULL,
            stock INTEGER NOT NULL,
            image_url TEXT NOT NULL,
            FOREIGN KEY (category_id) REFERENCES categories (id)
        )",
        [],
    )?;

    // Check if we need to seed data
    let count: i64 = conn.query_row("SELECT COUNT(*) FROM categories", [], |row| row.get(0))?;

    if count == 0 {
        seed_database(&conn)?;
    }

    Ok(conn)
}

fn seed_database(conn: &Connection) -> Result<()> {
    println!("🌱 Seeding database with sample data...");

    // Insert categories
    conn.execute(
        "INSERT INTO categories (name, description) VALUES (?1, ?2)",
        params!["Electronics", "Computers, phones, and electronic devices"],
    )?;
    conn.execute(
        "INSERT INTO categories (name, description) VALUES (?1, ?2)",
        params!["Books", "Physical and digital books"],
    )?;
    conn.execute(
        "INSERT INTO categories (name, description) VALUES (?1, ?2)",
        params!["Clothing", "Apparel and accessories"],
    )?;
    conn.execute(
        "INSERT INTO categories (name, description) VALUES (?1, ?2)",
        params!["Home & Garden", "Furniture, decor, and gardening supplies"],
    )?;

    // Insert products with real Unsplash photos
    let products = vec![
        ("Laptop Pro 15", "High-performance laptop with 16GB RAM", 1299.99, 1, 25, "https://images.unsplash.com/photo-1496181133206-80ce9b88a853?w=400"),
        ("Wireless Mouse", "Ergonomic wireless mouse with USB receiver", 29.99, 1, 150, "https://images.unsplash.com/photo-1527864550417-7fd91fc51a46?w=400"),
        ("Mechanical Keyboard", "RGB backlit mechanical keyboard", 89.99, 1, 75, "https://images.unsplash.com/photo-1587829741301-dc798b83add3?w=400"),
        ("4K Monitor", "27-inch 4K UHD display", 399.99, 1, 40, "https://images.unsplash.com/photo-1527443224154-c4a3942d3acf?w=400"),
        ("USB-C Hub", "7-in-1 USB-C hub with HDMI and Ethernet", 49.99, 1, 200, "https://images.unsplash.com/photo-1625948515291-69613efd103f?w=400"),

        ("Rust Programming Book", "The definitive guide to Rust", 39.99, 2, 100, "https://images.unsplash.com/photo-1532012197267-da84d127e765?w=400"),
        ("JavaScript Patterns", "Modern JavaScript design patterns", 34.99, 2, 80, "https://images.unsplash.com/photo-1544947950-fa07a98d237f?w=400"),
        ("Database Design", "SQL and NoSQL database fundamentals", 44.99, 2, 60, "https://images.unsplash.com/photo-1589998059171-988d887df646?w=400"),
        ("Clean Code", "A handbook of agile software craftsmanship", 42.99, 2, 90, "https://images.unsplash.com/photo-1512820790803-83ca734da794?w=400"),

        ("Cotton T-Shirt", "100% organic cotton, multiple colors", 19.99, 3, 500, "https://images.unsplash.com/photo-1521572163474-6864f9cf17ab?w=400"),
        ("Denim Jeans", "Classic fit denim jeans", 59.99, 3, 200, "https://images.unsplash.com/photo-1542272604-787c3835535d?w=400"),
        ("Running Shoes", "Lightweight running shoes", 79.99, 3, 120, "https://images.unsplash.com/photo-1542291026-7eec264c27ff?w=400"),
        ("Winter Jacket", "Waterproof insulated jacket", 149.99, 3, 50, "https://images.unsplash.com/photo-1551028719-00167b16eac5?w=400"),

        ("Coffee Table", "Modern wooden coffee table", 199.99, 4, 30, "https://images.unsplash.com/photo-1533090481720-856c6e3c1fdc?w=400"),
        ("LED Desk Lamp", "Adjustable LED lamp with USB charging", 39.99, 4, 85, "https://images.unsplash.com/photo-1507473885765-e6ed057f782c?w=400"),
        ("Indoor Plant Set", "Set of 3 easy-care indoor plants", 49.99, 4, 45, "https://images.unsplash.com/photo-1462530348824-f91d1a2a81dd?w=400"),
        ("Tool Set", "50-piece home repair tool set", 79.99, 4, 60, "https://images.unsplash.com/photo-1530124566582-a618bc2615dc?w=400"),
    ];

    for (name, desc, price, cat_id, stock, emoji) in products {
        conn.execute(
            "INSERT INTO products (name, description, price, category_id, stock, image_url)
             VALUES (?1, ?2, ?3, ?4, ?5, ?6)",
            params![name, desc, price, cat_id, stock, emoji],
        )?;
    }

    println!("✅ Database seeded successfully!");
    Ok(())
}

// Database queries
fn get_all_products(conn: &Connection, category_id: Option<i64>) -> Result<Vec<Product>> {
    let query = if let Some(cat_id) = category_id {
        format!(
            "SELECT p.id, p.name, p.description, p.price, p.category_id, c.name as category_name, p.stock, p.image_url
             FROM products p
             JOIN categories c ON p.category_id = c.id
             WHERE p.category_id = {}
             ORDER BY p.name",
            cat_id
        )
    } else {
        "SELECT p.id, p.name, p.description, p.price, p.category_id, c.name as category_name, p.stock, p.image_url
         FROM products p
         JOIN categories c ON p.category_id = c.id
         ORDER BY p.name".to_string()
    };

    let mut stmt = conn.prepare(&query)?;
    let products = stmt.query_map([], |row| {
        Ok(Product {
            id: row.get(0)?,
            name: row.get(1)?,
            description: row.get(2)?,
            price: row.get(3)?,
            category_id: row.get(4)?,
            category_name: row.get(5)?,
            stock: row.get(6)?,
            image_url: row.get(7)?,
        })
    })?;

    products.collect()
}

fn get_product_by_id(conn: &Connection, id: i64) -> Result<Product> {
    conn.query_row(
        "SELECT p.id, p.name, p.description, p.price, p.category_id, c.name as category_name, p.stock, p.image_url
         FROM products p
         JOIN categories c ON p.category_id = c.id
         WHERE p.id = ?1",
        params![id],
        |row| {
            Ok(Product {
                id: row.get(0)?,
                name: row.get(1)?,
                description: row.get(2)?,
                price: row.get(3)?,
                category_id: row.get(4)?,
                category_name: row.get(5)?,
                stock: row.get(6)?,
                image_url: row.get(7)?,
            })
        },
    )
}

fn search_products(conn: &Connection, query: &str) -> Result<Vec<Product>> {
    let search_pattern = format!("%{}%", query);
    let mut stmt = conn.prepare(
        "SELECT p.id, p.name, p.description, p.price, p.category_id, c.name as category_name, p.stock, p.image_url
         FROM products p
         JOIN categories c ON p.category_id = c.id
         WHERE p.name LIKE ?1 OR p.description LIKE ?1
         ORDER BY p.name",
    )?;

    let products = stmt.query_map(params![search_pattern], |row| {
        Ok(Product {
            id: row.get(0)?,
            name: row.get(1)?,
            description: row.get(2)?,
            price: row.get(3)?,
            category_id: row.get(4)?,
            category_name: row.get(5)?,
            stock: row.get(6)?,
            image_url: row.get(7)?,
        })
    })?;

    products.collect()
}

fn get_all_categories(conn: &Connection) -> Result<Vec<Category>> {
    let mut stmt = conn.prepare("SELECT id, name, description FROM categories ORDER BY name")?;
    let categories = stmt.query_map([], |row| {
        Ok(Category {
            id: row.get(0)?,
            name: row.get(1)?,
            description: row.get(2)?,
        })
    })?;

    categories.collect()
}

// HTTP helpers
fn parse_request(buffer: &[u8]) -> (String, String, bool) {
    let request = String::from_utf8_lossy(buffer);
    let lines: Vec<&str> = request.lines().collect();

    if lines.is_empty() {
        return (String::new(), String::new(), false);
    }

    let first_line: Vec<&str> = lines[0].split_whitespace().collect();
    let method = first_line.get(0).unwrap_or(&"").to_string();
    let uri = first_line.get(1).unwrap_or(&"").to_string();

    let is_switchback = lines.iter().any(|line| {
        line.to_lowercase().starts_with("x-switchback:")
    });

    (method, uri, is_switchback)
}

fn send_response(stream: &mut TcpStream, content_type: &str, body: &str) {
    let response = format!(
        "HTTP/1.1 200 OK\r\nContent-Type: {}\r\nContent-Length: {}\r\n\r\n{}",
        content_type,
        body.len(),
        body
    );
    let _ = stream.write_all(response.as_bytes());
}

fn send_json_response(stream: &mut TcpStream, json: &str) {
    send_response(stream, "application/json", json);
}

fn send_404(stream: &mut TcpStream) {
    let response = "HTTP/1.1 404 Not Found\r\nContent-Length: 13\r\n\r\n404 Not Found";
    let _ = stream.write_all(response.as_bytes());
}

fn serve_static(stream: &mut TcpStream, path: &str) {
    let file_path = if path.starts_with('/') {
        &path[1..]
    } else {
        path
    };

    match fs::read(file_path) {
        Ok(content) => {
            let content_type = if path.ends_with(".js") {
                "application/javascript"
            } else if path.ends_with(".wasm") {
                "application/wasm"
            } else if path.ends_with(".css") {
                "text/css"
            } else if path.ends_with(".html") {
                "text/html"
            } else if path.ends_with(".jpg") || path.ends_with(".jpeg") {
                "image/jpeg"
            } else if path.ends_with(".png") {
                "image/png"
            } else {
                "application/octet-stream"
            };

            let response = format!(
                "HTTP/1.1 200 OK\r\nContent-Type: {}\r\nContent-Length: {}\r\n\r\n",
                content_type,
                content.len()
            );

            let _ = stream.write_all(response.as_bytes());
            let _ = stream.write_all(&content);
        }
        Err(_) => send_404(stream),
    }
}

// Route handlers
fn handle_page_route(uri: &str) -> String {
    // Parse URI to extract route and params
    let (path, _query) = uri.split_once('?').unwrap_or((uri, ""));

    if path == "/" {
        format!(r#"{{"component":"Home","props":{{}},"url":"{}"}}"#, uri)
    } else if path.starts_with("/category/") {
        let category_id = path.strip_prefix("/category/").unwrap_or("1");
        format!(
            r#"{{"component":"CategoryPage","props":{{"categoryId":{}}},"url":"{}"}}"#,
            category_id, uri
        )
    } else if path.starts_with("/product/") {
        let product_id = path.strip_prefix("/product/").unwrap_or("1");
        format!(
            r#"{{"component":"ProductPage","props":{{"productId":{}}},"url":"{}"}}"#,
            product_id, uri
        )
    } else if path == "/search" {
        format!(r#"{{"component":"SearchPage","props":{{}},"url":"{}"}}"#, uri)
    } else {
        format!(r#"{{"component":"Home","props":{{}},"url":"/"}}"#)
    }
}

fn handle_client(mut stream: TcpStream, db: Arc<Mutex<Connection>>) {
    let mut buffer = [0u8; BUFFER_SIZE];

    match stream.read(&mut buffer) {
        Ok(bytes_read) if bytes_read > 0 => {
            let (method, uri, is_switchback) = parse_request(&buffer[..bytes_read]);

            // Serve static files
            if uri.starts_with("/dist/") || uri.starts_with("/pkg/") {
                serve_static(&mut stream, &uri);
                return;
            }

            // API endpoints
            if uri.starts_with("/api/") {
                let conn = db.lock().unwrap();

                if uri == "/api/products" {
                    match get_all_products(&conn, None) {
                        Ok(products) => {
                            let json = serde_json::to_string(&products).unwrap();
                            send_json_response(&mut stream, &json);
                        }
                        Err(e) => {
                            eprintln!("Database error: {}", e);
                            send_404(&mut stream);
                        }
                    }
                } else if uri.starts_with("/api/products/") {
                    let id_str = uri.strip_prefix("/api/products/").unwrap();
                    if let Ok(id) = id_str.parse::<i64>() {
                        match get_product_by_id(&conn, id) {
                            Ok(product) => {
                                let json = serde_json::to_string(&product).unwrap();
                                send_json_response(&mut stream, &json);
                            }
                            Err(_) => send_404(&mut stream),
                        }
                    } else {
                        send_404(&mut stream);
                    }
                } else if uri.starts_with("/api/category/") {
                    let parts: Vec<&str> = uri.split('/').collect();
                    if parts.len() >= 4 {
                        if let Ok(cat_id) = parts[3].parse::<i64>() {
                            match get_all_products(&conn, Some(cat_id)) {
                                Ok(products) => {
                                    let json = serde_json::to_string(&products).unwrap();
                                    send_json_response(&mut stream, &json);
                                }
                                Err(e) => {
                                    eprintln!("Database error: {}", e);
                                    send_404(&mut stream);
                                }
                            }
                        }
                    }
                } else if uri.starts_with("/api/search?q=") {
                    let query = uri.strip_prefix("/api/search?q=").unwrap();
                    let decoded = urlencoding::decode(query).unwrap_or_default();
                    match search_products(&conn, &decoded) {
                        Ok(products) => {
                            let json = serde_json::to_string(&products).unwrap();
                            send_json_response(&mut stream, &json);
                        }
                        Err(e) => {
                            eprintln!("Database error: {}", e);
                            send_404(&mut stream);
                        }
                    }
                } else if uri == "/api/categories" {
                    match get_all_categories(&conn) {
                        Ok(categories) => {
                            let json = serde_json::to_string(&categories).unwrap();
                            send_json_response(&mut stream, &json);
                        }
                        Err(e) => {
                            eprintln!("Database error: {}", e);
                            send_404(&mut stream);
                        }
                    }
                }
                return;
            }

            // Handle page routes
            if method == "GET" {
                let page_json = handle_page_route(&uri);

                if is_switchback {
                    send_response(&mut stream, "application/json", &page_json);
                } else {
                    let html = format!(
                        r#"<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Rust Product Catalog - Embedded SQLite Demo</title>
    <script>window.initialPage = {};</script>
</head>
<body>
    <div data-swbk-app>
        <div style="padding: 2rem; text-align: center; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); color: white; min-height: 100vh; display: flex; align-items: center; justify-content: center;">
            <div>
                <p style="font-size: 2rem; margin-bottom: 1rem;">⏳</p>
                <p style="font-size: 1.2rem;">Loading Switchback app...</p>
            </div>
        </div>
    </div>
    <script type="module" src="/dist/app.js"></script>
</body>
</html>"#,
                        page_json
                    );
                    send_response(&mut stream, "text/html", &html);
                }
            }
        }
        _ => {}
    }
}

fn main() {
    // Initialize database
    let conn = init_database().expect("Failed to initialize database");
    let db = Arc::new(Mutex::new(conn));

    // Setup graceful shutdown
    let running = Arc::new(AtomicBool::new(true));
    let r = running.clone();

    let mut signals = signal_hook::iterator::Signals::new(&[
        signal_hook::consts::SIGTERM,
        signal_hook::consts::SIGINT,
    ])
    .expect("Error setting signal handlers");

    thread::spawn(move || {
        for sig in signals.forever() {
            println!("\n🦀 Shutting down gracefully... (signal: {})", sig);
            r.store(false, Ordering::SeqCst);
            break;
        }
    });

    let listener = TcpListener::bind(format!("0.0.0.0:{}", PORT))
        .expect("Failed to bind to port");

    listener
        .set_nonblocking(true)
        .expect("Cannot set non-blocking");

    println!("🦀 Rust server listening on http://0.0.0.0:{}", PORT);
    println!("   Product Catalog with Embedded SQLite!");
    println!("   📦 Single binary - no external database needed");
    println!("   Visit http://localhost:{} to see it in action", PORT);

    while running.load(Ordering::SeqCst) {
        match listener.accept() {
            Ok((stream, _)) => {
                let db_clone = Arc::clone(&db);
                thread::spawn(move || {
                    handle_client(stream, db_clone);
                });
            }
            Err(ref e) if e.kind() == std::io::ErrorKind::WouldBlock => {
                thread::sleep(Duration::from_millis(100));
                continue;
            }
            Err(e) => {
                eprintln!("Connection error: {}", e);
            }
        }
    }

    println!("🦀 Server stopped");
}
