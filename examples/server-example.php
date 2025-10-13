<?php
/**
 * Example PHP backend for Switchback
 *
 * This demonstrates how to integrate Switchback with a traditional
 * server-side application (in this case, PHP - but works with any backend)
 */

// Simple routing
$method = $_SERVER['REQUEST_METHOD'];
$uri = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
$isInertia = isset($_SERVER['HTTP_X_SWITCHBACK']) || isset($_SERVER['HTTP_X_INERTIA']);

// Fake database
$users = [
    ['id' => 1, 'name' => 'John Doe', 'email' => 'john@example.com', 'joined' => '2024-01-15'],
    ['id' => 2, 'name' => 'Jane Smith', 'email' => 'jane@example.com', 'joined' => '2024-02-20'],
    ['id' => 3, 'name' => 'Bob Johnson', 'email' => 'bob@example.com', 'joined' => '2024-03-10'],
];

// Helper to return Switchback response
function switchback_response($component, $props) {
    global $isInertia;

    $page = [
        'component' => $component,
        'props' => $props,
        'url' => $_SERVER['REQUEST_URI'],
    ];

    if ($isInertia) {
        header('Content-Type: application/json');
        echo json_encode($page);
        exit;
    }

    return $page;
}

// Routes
if ($uri === '/' && $method === 'GET') {
    $page = switchback_response('Home', [
        'title' => 'Welcome to Switchback',
        'stats' => [
            'users' => count($users),
            'posts' => 42,
        ],
    ]);
}
elseif ($uri === '/users' && $method === 'GET') {
    $page = switchback_response('Users/Index', [
        'users' => $users,
    ]);
}
elseif (preg_match('/^\/users\/(\d+)$/', $uri, $matches) && $method === 'GET') {
    $id = (int) $matches[1];
    $user = array_values(array_filter($users, fn($u) => $u['id'] === $id))[0] ?? null;

    if (!$user) {
        http_response_code(404);
        $page = switchback_response('Error', ['message' => 'User not found']);
    } else {
        $page = switchback_response('Users/Show', ['user' => $user]);
    }
}
elseif (preg_match('/^\/users\/(\d+)\/edit$/', $uri, $matches) && $method === 'GET') {
    $id = (int) $matches[1];
    $user = array_values(array_filter($users, fn($u) => $u['id'] === $id))[0] ?? null;

    if (!$user) {
        http_response_code(404);
        $page = switchback_response('Error', ['message' => 'User not found']);
    } else {
        $page = switchback_response('Users/Form', ['user' => $user]);
    }
}
elseif ($uri === '/users/new' && $method === 'GET') {
    $page = switchback_response('Users/Form', [
        'user' => null,
        'errors' => null,
    ]);
}
elseif ($uri === '/users' && $method === 'POST') {
    // Validation
    $errors = [];
    if (empty($_POST['name'])) $errors['name'] = 'Name is required';
    if (empty($_POST['email'])) $errors['email'] = 'Email is required';

    if ($errors) {
        http_response_code(422);
        $page = switchback_response('Users/Form', [
            'user' => ['name' => $_POST['name'] ?? '', 'email' => $_POST['email'] ?? ''],
            'errors' => $errors,
        ]);
    } else {
        // Save user (in real app, would save to database)
        $page = switchback_response('Users/Index', [
            'users' => $users,
            'flash' => [
                'type' => 'success',
                'message' => 'User created successfully!',
            ],
        ]);
    }
}
elseif (preg_match('/^\/users\/(\d+)$/', $uri, $matches) && $method === 'POST') {
    $id = (int) $matches[1];

    // Validation
    $errors = [];
    if (empty($_POST['name'])) $errors['name'] = 'Name is required';
    if (empty($_POST['email'])) $errors['email'] = 'Email is required';

    if ($errors) {
        http_response_code(422);
        $page = switchback_response('Users/Form', [
            'user' => array_merge(['id' => $id], $_POST),
            'errors' => $errors,
        ]);
    } else {
        // Update user (in real app, would save to database)
        $page = switchback_response('Users/Show', [
            'user' => [
                'id' => $id,
                'name' => $_POST['name'],
                'email' => $_POST['email'],
                'joined' => '2024-01-15',
            ],
            'flash' => [
                'type' => 'success',
                'message' => 'User updated successfully!',
            ],
        ]);
    }
}
elseif (preg_match('/^\/users\/(\d+)$/', $uri, $matches) && $method === 'DELETE') {
    // Delete user (in real app, would delete from database)
    $page = switchback_response('Users/Index', [
        'users' => $users,
        'flash' => [
            'type' => 'success',
            'message' => 'User deleted successfully!',
        ],
    ]);
}
elseif ($uri === '/about' && $method === 'GET') {
    $page = switchback_response('About', [
        'version' => '0.1.0',
        'features' => [
            'Zero dependencies',
            'Vendorable',
            'Auditable (~200 lines)',
            'Works with any backend',
            'Inertia-inspired',
        ],
    ]);
}
else {
    http_response_code(404);
    $page = switchback_response('Error', ['message' => 'Page not found']);
}

// If not Switchback request, render full HTML
?>
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Switchback Example</title>
    <style>
        * { margin: 0; padding: 0; box-sizing: border-box; }
        body { font-family: system-ui, sans-serif; line-height: 1.6; }
        nav { background: #333; color: white; padding: 1rem; display: flex; gap: 1rem; }
        nav a { color: white; text-decoration: none; }
        nav a:hover { text-decoration: underline; }
        main { padding: 2rem; max-width: 800px; margin: 0 auto; }
        .flash { padding: 1rem; margin-bottom: 1rem; border-radius: 4px; display: flex; justify-content: space-between; }
        .flash-success { background: #d4edda; color: #155724; border: 1px solid #c3e6cb; }
        .flash-error { background: #f8d7da; color: #721c24; border: 1px solid #f5c6cb; }
        .flash button { background: none; border: none; font-size: 1.5rem; cursor: pointer; }
        .user-list { display: grid; gap: 1rem; margin: 1rem 0; }
        .user-card { border: 1px solid #ddd; padding: 1rem; border-radius: 4px; }
        .form-group { margin-bottom: 1rem; }
        label { display: block; margin-bottom: 0.5rem; font-weight: bold; }
        input { width: 100%; padding: 0.5rem; border: 1px solid #ddd; border-radius: 4px; }
        input.error { border-color: #dc3545; }
        .error-message { color: #dc3545; font-size: 0.875rem; }
        button { background: #007bff; color: white; border: none; padding: 0.5rem 1rem; border-radius: 4px; cursor: pointer; }
        button:hover { background: #0056b3; }
        .form-actions { display: flex; gap: 1rem; }
        a { color: #007bff; text-decoration: none; }
        a:hover { text-decoration: underline; }
    </style>
    <script>
        window.initialPage = <?= json_encode($page) ?>;
    </script>
</head>
<body>
    <div data-switchback-app></div>
    <script type="module" src="/examples/app.js"></script>
</body>
</html>
