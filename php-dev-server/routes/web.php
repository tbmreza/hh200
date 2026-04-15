<?php

use App\Http\Controllers\EchoController;
use App\Http\Controllers\XlsUploadController;
use Illuminate\Support\Facades\Route;

Route::any('/echo', EchoController::class);
Route::post('/xls', XlsUploadController::class);

Route::get('/', function () {
    return view('welcome');
});
