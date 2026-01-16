<?php

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;
use App\Http\Controllers\ApiController;

Route::get('/user', function (Request $request) {
    return $request->user();
})->middleware('auth:sanctum');

Route::any('/echo', [ApiController::class, 'echo']);
Route::post('/mkpdf', [ApiController::class, 'mkpdf']);