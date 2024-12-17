// <?php
// // Get the raw POST data
// $requestBody = file_get_contents('php://input');
//
// // Print the request body
// echo "Request Body: " . $requestBody . "\n";
//
// // Get all headers
// $headers = getallheaders();
//
// // Print the headers
// echo "Request Headers:\n";
// foreach ($headers as $key => $value) {
//     echo "$key: $value\n";
// }
// ?>
<?php
// Enable error reporting for debugging
error_reporting(E_ALL);
ini_set('display_errors', 1);

// Set content type to plain text for easy reading
header('Content-Type: text/plain');

// Function to get all request headers (works across different server configurations)
function getRequestHeaders() {
    $headers = [];
    
    // Apache or nginx specific method
    if (function_exists('getallheaders')) {
        $headers = getallheaders();
    } 
    // Alternative method for other servers
    else {
        foreach ($_SERVER as $name => $value) {
            if (substr($name, 0, 5) == 'HTTP_') {
                $name = str_replace(' ', '-', ucwords(strtolower(str_replace('_', ' ', substr($name, 5)))));
                $headers[$name] = $value;
            }
        }
    }
    
    return $headers;
}

// Capture request method
echo "Request Method: " . $_SERVER['REQUEST_METHOD'] . "\n\n";

// Print request headers
echo "--- REQUEST HEADERS ---\n";
$headers = getRequestHeaders();
foreach ($headers as $name => $value) {
    echo "$name: $value\n";
}

echo "\n--- REQUEST BODY ---\n";
// Read raw request body
$rawBody = file_get_contents('php://input');
echo $rawBody ?: "No request body found.";

echo "\n\n--- ADDITIONAL REQUEST INFO ---\n";
echo "Query String: " . ($_SERVER['QUERY_STRING'] ?? 'No query string') . "\n";
echo "Content Type: " . ($_SERVER['CONTENT_TYPE'] ?? 'Not specified') . "\n";
echo "Content Length: " . ($_SERVER['CONTENT_LENGTH'] ?? 'Not specified') . "\n";
