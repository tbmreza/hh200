<?php

namespace App\Http\Middleware;

use Closure;
use Illuminate\Http\Request;
use Symfony\Component\HttpFoundation\Response;

class LogRequestToConsole
{
    public function handle(Request $request, Closure $next): Response
    {
        $data = [
            'method' => $request->method(),
            'url' => $request->fullUrl(),
            'headers' => $request->headers->all(),
            'query' => $request->query(),
            'body' => $request->all(),
            'content' => $request->getContent(),
        ];

        $output = fopen('php://stdout', 'w');
        fwrite($output, "\n--- Incoming Request ---\n");
        fwrite($output, json_encode($data, JSON_PRETTY_PRINT));
        fwrite($output, "\n------------------------\n");
        fclose($output);

        return $next($request);
    }
}
