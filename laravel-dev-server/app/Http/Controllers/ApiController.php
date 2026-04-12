<?php

namespace App\Http\Controllers;

use Barryvdh\DomPDF\Facade\Pdf;
use Illuminate\Http\Request;

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

    public function login(Request $request)
    {
        $request->validate([
            'email' => 'required|email',
            'password' => 'required',
        ]);

        $user = \App\Models\User::where('email', $request->email)->first();

        if (! $user || ! Hash::check($request->password, $user->password)) {
            return response()->json([
                'data' => ['token' => null],
            ], 401);
        }

        $key = config('app.key');
        $payload = [
            'sub' => $user->id,
            'email' => $user->email,
            'iat' => time(),
            'exp' => time() + 3600,
        ];

        $token = JWT::encode($payload, $key, 'HS256');

        return response()->json([
            'data' => ['token' => $token],
        ]);
    }

    public function mkpdf(Request $request)
    {
        $content = $request->input('content', 'No content provided');
        $pdf = Pdf::loadHTML($content);

        return $pdf->download('document.pdf');
    }
}
