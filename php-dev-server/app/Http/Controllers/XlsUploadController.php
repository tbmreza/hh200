<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Illuminate\Validation\ValidationException;
use Illuminate\Support\Facades\Storage;

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
            $data = [
                'method' => $request->method(),
                'url' => $request->fullUrl(),
                'headers' => $request->headers->all(),
                'query' => $request->query(),
                'body' => $request->all(),
                'content' => $request->getContent(),
            ];

            $output = fopen('php://stdout', 'w');
            fwrite($output, "\n--- Echo Request ---\n");
            fwrite($output, json_encode($data, JSON_PRETTY_PRINT));
            fwrite($output, "\n--------------------\n");
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
}
