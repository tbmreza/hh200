<?php

use App\Http\Controllers\AuthController;
use App\Http\Controllers\EchoController;
use App\Http\Controllers\XlsUploadController;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;

Route::post('/login', [AuthController::class, 'login']);
Route::any('/echo', EchoController::class);
Route::post('/xls', XlsUploadController::class);

Route::get('/user', function (Request $request) {
    return $request->user();
})->middleware('auth:api');
