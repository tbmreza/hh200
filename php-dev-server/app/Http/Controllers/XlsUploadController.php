<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Storage;

class XlsUploadController extends Controller
{
    /**
     * Handle the Excel file upload.
     */
    public function __invoke(Request $request)
    {
        $request->validate([
            'file' => 'required|file',
        ]);

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
