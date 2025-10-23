<?php
/**
 * PHP Recipe - Switchback Integration
 *
 * A minimal example showing how to integrate Switchback with vanilla PHP.
 * No frameworks needed - just plain PHP serving JSON for Switchback requests.
 */

// Serve static files (for PHP built-in server)
if (php_sapi_name() === 'cli-server') {
    $file = __DIR__ . $_SERVER['REQUEST_URI'];
    if (is_file($file)) {
        return false; // Let PHP's built-in server handle the file
    }
}

// Mock database
$users = [
    ['id' => 1, 'name' => 'Alice Johnson', 'email' => 'alice@example.com', 'joined' => '2024-01-15'],
    ['id' => 2, 'name' => 'Bob Smith', 'email' => 'bob@example.com', 'joined' => '2024-02-20'],
    ['id' => 3, 'name' => 'Charlie Brown', 'email' => 'charlie@example.com', 'joined' => '2024-03-10'],
];

// Get request info
$method = $_SERVER['REQUEST_METHOD'];
$uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
$isSwitchback = isset($_SERVER['HTTP_X_SWITCHBACK']);

// Helper to send Switchback response
function respond($component, $props, $flash = null) {
    global $isSwitchback;

    if ($flash) {
        $props['flash'] = $flash;
    }

    $page = [
        'component' => $component,
        'props' => $props,
        'url' => $_SERVER['REQUEST_URI'],
    ];

    if ($isSwitchback) {
        header('Content-Type: application/json');
        echo json_encode($page);
        exit;
    }

    return $page;
}

// Router
if ($uri === '/' || $uri === '/index.php') {
    $page = respond('Home', [
        'title' => 'PHP Recipe - Switchback',
        'message' => 'This is a plain PHP backend with Switchback integration',
        'stats' => [
            'users' => count($users),
            'framework' => 'None - Vanilla PHP',
        ],
    ]);
}
elseif ($uri === '/users' && $method === 'GET') {
    $page = respond('Users/Index', ['users' => $users]);
}
elseif (preg_match('#^/users/(\d+)$#', $uri, $matches) && $method === 'GET') {
    $id = (int)$matches[1];
    $user = current(array_filter($users, fn($u) => $u['id'] === $id)) ?: null;

    if (!$user) {
        http_response_code(404);
        $page = respond('Error', ['message' => 'User not found']);
    } else {
        $page = respond('Users/Show', ['user' => $user]);
    }
}
elseif ($uri === '/about' && $method === 'GET') {
    $page = respond('About', [
        'version' => '1.0.0',
        'backend' => 'Vanilla PHP',
        'features' => [
            'No framework required',
            'Built-in PHP server support',
            'Simple routing',
            'JSON responses for Switchback',
        ],
    ]);
}
else {
    http_response_code(404);
    $page = respond('Error', ['message' => 'Page not found']);
}

// Render HTML template (only for non-Switchback requests)
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>PHP Recipe - Switchback</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body {
            font-family: system-ui, -apple-system, sans-serif;
            line-height: 1.6;
            color: #333;
        }
        nav {
            background: #2c3e50;
            color: white;
            padding: 1rem 2rem;
            position: relative;
        }
        .nav-content {
            display: flex;
            justify-content: space-between;
            align-items: center;
        }
        .nav-badge {
            background: rgba(76, 175, 80, 0.2);
            padding: 0.25rem 0.75rem;
            border-radius: 4px;
            font-size: 0.875rem;
            border: 1px solid rgba(76, 175, 80, 0.5);
            cursor: help;
        }
        nav a {
            color: white;
            text-decoration: none;
            padding: 0.5rem 1rem;
            border-radius: 4px;
            transition: background 0.2s;
            margin-right: 0.5rem;
        }
        nav a:hover { background: rgba(255, 255, 255, 0.1); }
        main {
            max-width: 900px;
            margin: 0 auto;
            padding: 2rem;
        }
        h1 { color: #2c3e50; margin-bottom: 1rem; }
        .badge {
            display: inline-block;
            background: #3498db;
            color: white;
            padding: 0.25rem 0.75rem;
            border-radius: 4px;
            font-size: 0.875rem;
            margin-left: 0.5rem;
        }
        .stats {
            display: grid;
            grid-template-columns: repeat(auto-fit, minmax(200px, 1fr));
            gap: 1rem;
            margin: 1.5rem 0;
        }
        .stat-card {
            padding: 1.5rem;
            background: #f8f9fa;
            border: 1px solid #dee2e6;
            border-radius: 8px;
            text-align: center;
        }
        .stat-card strong {
            display: block;
            font-size: 2rem;
            color: #3498db;
            margin-bottom: 0.5rem;
        }
        .user-list { display: grid; gap: 1rem; margin: 1rem 0; }
        .user-card {
            border: 1px solid #dee2e6;
            padding: 1.5rem;
            border-radius: 8px;
            background: white;
            transition: box-shadow 0.2s;
        }
        .user-card:hover { box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
        .user-card h3 { margin-bottom: 0.5rem; }
        .user-card a { color: #2c3e50; text-decoration: none; }
        .user-card a:hover { color: #3498db; text-decoration: underline; }
        .user-card p { color: #6c757d; }
        a.btn {
            display: inline-block;
            background: #3498db;
            color: white;
            padding: 0.75rem 1.5rem;
            text-decoration: none;
            border-radius: 4px;
            transition: background 0.2s;
        }
        a.btn:hover { background: #2980b9; }
        ul { list-style-position: inside; margin: 1rem 0; }
        li { margin: 0.5rem 0; }
    </style>
    <script>
        window.initialPage = <?= json_encode($page) ?>;
    </script>
</head>
<body>
    <div data-swbk-app>
        <div style="padding: 2rem; text-align: center;">
            <p>Loading Switchback app...</p>
        </div>
    </div>
    <script type="module" src="/dist/app.js"></script>
</body>
</html>
