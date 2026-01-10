<?php
require 'vendor/autoload.php';

// Set headers for CORS and JSON input
header('Access-Control-Allow-Origin: *');
header('Access-Control-Allow-Methods: POST, OPTIONS');
header('Access-Control-Allow-Headers: Content-Type');

if ($_SERVER['REQUEST_METHOD'] === 'OPTIONS') {
    http_response_code(200);
    exit;
}

if ($_SERVER['REQUEST_METHOD'] !== 'POST') {
    http_response_code(405);
    echo "Method Not Allowed";
    exit;
}

// Get JSON input
$input = file_get_contents('php://input');
$data = json_decode($input, true);

// ??: in symfony, fix 400; proper routing for echo.php and mkpdf.php
if (!$data || empty($data['title']) || empty($data['content'])) {
    http_response_code(400);
    header('Content-Type: application/json');
    echo json_encode(['error' => 'Bad Request: "title" and "content" are required fields.']);
    exit;
}

$title = $data['title'];
$content = $data['content'];

// Create PDF
use Fpdf\Fpdf;
$pdf = new Fpdf();
$pdf->AddPage();

// Title
$pdf->SetFont('Arial', 'B', 16);
$pdf->Cell(0, 10, utf8_decode($title), 0, 1, 'C');
$pdf->Ln(10);

// Content
$pdf->SetFont('Arial', '', 12);
$pdf->MultiCell(0, 10, utf8_decode($content));

// Output PDF
// 'D' forces download, 'document.pdf' is the default filename
$pdf->Output('D', 'document.pdf');
?>
