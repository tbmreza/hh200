<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Illuminate\Validation\ValidationException;
use Illuminate\Support\Facades\Storage;
use Illuminate\Support\Str;

class XlsUploadController extends Controller
{
    /**
     * Handle the Excel file upload.
     */
    public function __invoke(Request $request)
    {
        try {
            $request->validate([
                'file' => 'required|file',
            ]);
        } catch (ValidationException $e) {
            $output = fopen('php://stdout', 'w');
            fwrite($output, "\n========================================\n");
            fwrite($output, "  422 Unprocessable Content - /api/xls\n");
            fwrite($output, "========================================\n");
            fwrite($output, "Method: " . $request->method() . "\n");
            fwrite($output, "URL: " . $request->fullUrl() . "\n");
            fwrite($output, "Content-Type: " . $request->header('Content-Type', 'not set') . "\n");
            fwrite($output, "\nValidation Errors:\n");
            foreach ($e->errors() as $field => $messages) {
                foreach ($messages as $message) {
                    fwrite($output, "  - {$field}: {$message}\n");
                }
            }
            fwrite($output, "\nHeaders:\n");
            foreach ($request->headers->all() as $key => $values) {
                fwrite($output, "  {$key}: " . implode(', ', $values) . "\n");
            }
            fwrite($output, "\nQuery Parameters:\n");
            $query = $request->query();
            if (empty($query)) {
                fwrite($output, "  (none)\n");
            } else {
                foreach ($query as $key => $value) {
                    fwrite($output, "  {$key}: {$value}\n");
                }
            }
            fwrite($output, "\nRequest Body (parsed):\n");
            $body = $request->all();
            if (empty($body)) {
                fwrite($output, "  (empty)\n");
            } else {
                fwrite($output, json_encode($body, JSON_PRETTY_PRINT) . "\n");
            }
            fwrite($output, "\nRaw Content:\n");
            $content = $request->getContent();
            if (empty($content)) {
                fwrite($output, "  (empty)\n");
            } else {
                fwrite($output, $content . "\n");
            }
            fwrite($output, "========================================\n\n");
            fclose($output);

            throw $e;
        }

        if ($request->file('file')->isValid()) {
            $path = $request->file('file')->store('uploads');

            return response()->json([
                'message' => 'File uploaded successfully',
                'path' => $path,
                'name' => $request->file('file')->getClientOriginalName(),
                'size' => $request->file('file')->getSize(),
            ]);
        }

        return response()->json(['error' => 'Invalid file upload'], 400);
    }

    /**
     * List all uploaded files.
     */
    public function index()
    {
        $files = Storage::disk('local')->files('uploads');

        return response()->json($files);
    }

    /**
     * Download a file by its ID (path).
     */
    public function download($id)
    {
        // Validate that the ID starts with 'uploads/' to prevent directory traversal
        if (!Str::startsWith($id, 'uploads/')) {
            return response()->json(['error' => 'Invalid file ID'], 400);
        }

        if (Storage::disk('local')->exists($id)) {
            return Storage::disk('local')->download($id);
        }

        return response()->json(['error' => 'File not found'], 404);
    }
}