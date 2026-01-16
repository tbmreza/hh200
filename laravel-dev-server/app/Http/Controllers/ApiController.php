<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Barryvdh\DomPDF\Facade\Pdf;

class ApiController extends Controller
{
    public function echo(Request $request)
    {
        return response()->json([
            'url' => $request->fullUrl(),
            'method' => $request->method(),
            'params' => $request->query(),
            'headers' => $request->headers->all(),
            'body' => $request->all(),
            'ip' => $request->ip(),
        ]);
    }

    public function mkpdf(Request $request)
    {
        $content = $request->input('content', 'No content provided');
        $pdf = Pdf::loadHTML($content);
        return $pdf->download('document.pdf');
    }
}