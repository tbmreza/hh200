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

        return response()->json($data);
    }
}
