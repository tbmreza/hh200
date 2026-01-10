<?php
header('Content-Type: text/plain');
header('Access-Control-Allow-Origin: *');
header('Access-Control-Allow-Methods: GET, POST, PUT, DELETE, PATCH, OPTIONS');
header('Access-Control-Allow-Headers: *');

// Handle preflight OPTIONS requests
if ($_SERVER['REQUEST_METHOD'] === 'OPTIONS') {
    http_response_code(200);
    exit;
}

echo "=== PHP ECHO SERVER ===\n\n";

// Request Method and URI
echo "METHOD: " . $_SERVER['REQUEST_METHOD'] . "\n";
echo "URI: " . $_SERVER['REQUEST_URI'] . "\n";
echo "PROTOCOL: " . $_SERVER['SERVER_PROTOCOL'] . "\n\n";

// Headers
echo "=== HEADERS ===\n";
$headers = getallheaders();
if ($headers) {
    foreach ($headers as $name => $value) {
        echo "$name: $value\n";
    }
} else {
    // Fallback for servers where getallheaders() isn't available
    foreach ($_SERVER as $key => $value) {
        if (substr($key, 0, 5) === 'HTTP_') {
            $header = str_replace('_', '-', substr($key, 5));
            echo "$header: $value\n";
        }
    }
}
echo "\n";

// Query Parameters
if (!empty($_GET)) {
    echo "=== QUERY PARAMETERS ===\n";
    foreach ($_GET as $key => $value) {
        if (is_array($value)) {
            echo "$key: " . implode(', ', $value) . "\n";
        } else {
            echo "$key: $value\n";
        }
    }
    echo "\n";
}

// POST/Form Data
if (!empty($_POST)) {
    echo "=== FORM DATA ===\n";
    foreach ($_POST as $key => $value) {
        if (is_array($value)) {
            echo "$key: " . implode(', ', $value) . "\n";
        } else {
            echo "$key: $value\n";
        }
    }
    echo "\n";
}

// Raw Request Body
$body = file_get_contents('php://input');
if (!empty($body)) {
    echo "=== RAW REQUEST BODY ===\n";
    echo "Length: " . strlen($body) . " bytes\n";
    echo "Content:\n";
    echo $body . "\n\n";
}

// Files (if any)
if (!empty($_FILES)) {
    echo "=== UPLOADED FILES ===\n";
    foreach ($_FILES as $key => $file) {
        echo "Field: $key\n";
        echo "  Name: " . $file['name'] . "\n";
        echo "  Type: " . $file['type'] . "\n";
        echo "  Size: " . $file['size'] . " bytes\n";
        echo "  Tmp: " . $file['tmp_name'] . "\n";
        echo "  Error: " . $file['error'] . "\n\n";
    }
}

// Server Information
echo "=== SERVER INFO ===\n";
echo "Server Software: " . ($_SERVER['SERVER_SOFTWARE'] ?? 'Unknown') . "\n";
echo "Server Name: " . $_SERVER['SERVER_NAME'] . "\n";
echo "Server Port: " . $_SERVER['SERVER_PORT'] . "\n";
echo "Remote Address: " . $_SERVER['REMOTE_ADDR'] . "\n";
echo "Remote Port: " . ($_SERVER['REMOTE_PORT'] ?? 'Unknown') . "\n";
echo "Request Time: " . date('Y-m-d H:i:s', $_SERVER['REQUEST_TIME']) . "\n\n";

// Additional Environment Variables (optional)
echo "=== ADDITIONAL INFO ===\n";
echo "Script Name: " . $_SERVER['SCRIPT_NAME'] . "\n";
echo "Document Root: " . $_SERVER['DOCUMENT_ROOT'] . "\n";
echo "HTTPS: " . (isset($_SERVER['HTTPS']) && $_SERVER['HTTPS'] === 'on' ? 'Yes' : 'No') . "\n";

// User Agent (if available)
if (isset($_SERVER['HTTP_USER_AGENT'])) {
    echo "User Agent: " . $_SERVER['HTTP_USER_AGENT'] . "\n";
}

echo "\n=== END ===\n";
?>
