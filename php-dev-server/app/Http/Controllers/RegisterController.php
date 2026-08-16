<?php

namespace App\Http\Controllers;

use App\Models\XlsFile;
use Illuminate\Http\Request;

class RegisterController extends Controller
{
    public function __invoke(Request $request)
    {
        $request->validate([
            'description' => 'required|string',
            'file' => 'required|file',
        ]);

        if ($request->file('file')->isValid()) {
            $path = $request->file('file')->store('uploads');
            $file = $request->file('file');

            $xlsFile = XlsFile::create([
                'filename' => pathinfo($file->getClientOriginalName(), PATHINFO_FILENAME),
                'original_name' => $file->getClientOriginalName(),
                'path' => $path,
                'size' => $file->getSize(),
            ]);

            return response()->json([
                'message' => 'File uploaded successfully',
                'id' => $xlsFile->id,
                'name' => $file->getClientOriginalName(),
                'size' => $file->getSize(),
                'description' => $request->input('description'),
            ]);
        }

        return response()->json(['error' => 'Invalid file upload'], 400);
    }
}