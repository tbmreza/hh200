<?php

use App\Http\Controllers\AuthController;
use App\Http\Controllers\EchoController;
use App\Http\Controllers\XlsUploadController;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\Route;

Route::post('/login', [AuthController::class, 'login']);
Route::any('/echo', EchoController::class);
Route::post('/xls', XlsUploadController::class);
Route::get('/xls', [XlsUploadController::class, 'index']);
Route::get('/xls/{id}', [XlsUploadController::class, 'download']);
Route::delete('/xls/{id}', [XlsUploadController::class, 'destroy']);

Route::get('/user', function (Request $request) {
    return $request->user();
})->middleware('auth:api');
