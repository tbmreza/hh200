<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Log;

class EchoController extends Controller
{
    /**
     * Echo the request details.
     */
    public function __invoke(Request $request)
    {
        $data = [
            'method' => $request->method(),
            'url' => $request->fullUrl(),
            'headers' => $request->headers->all(),
            'query' => $request->query(),
            'body' => $request->all(),
            'content' => $request->getContent(),
        ];

        // Print to console (stdout)
        $output = fopen('php://stdout', 'w');
        fwrite($output, "\n--- Echo Request ---\n");
        fwrite($output, json_encode($data, JSON_PRETTY_PRINT));
        fwrite($output, "\n--------------------\n");
        fclose($output);

        return response()->json($data);
    }
}
