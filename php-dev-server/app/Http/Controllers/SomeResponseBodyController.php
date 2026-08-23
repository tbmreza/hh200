<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

class SomeResponseBodyController extends Controller
{
    /**
     * Return a dummy response body depending on the toggle, with the requested status code.
     */
    public function __invoke(Request $request, string $toggle, int $status_code)
    {
        $toggleBool = filter_var($toggle, FILTER_VALIDATE_BOOLEAN);

        if (! $toggleBool) {
            return response()->noContent($status_code);
        }

        return response()->json([
            'message' => 'Toggle is true',
            'data' => ['example' => 'value'],
        ], $status_code);
    }
}
