<?php

use App\Http\Controllers\ApiController;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;

Route::get('/user', function (Request $request) {
    return $request->user();
})->middleware('auth:sanctum');

Route::any('/echo', [ApiController::class, 'echo']);
Route::post('/login', [ApiController::class, 'login']);
Route::post('/mkpdf', [ApiController::class, 'mkpdf']);
