<?php

use App\Http\Controllers\WebDavController;
use Illuminate\Support\Facades\Route;

Route::get('/', function () {
    return view('welcome');
});

Route::match(['GET', 'PROPFIND'], '{path?}', [WebDavController::class, 'propfind'])
    ->where('path', '.*')
    ->middleware('web');

Route::match(['MKCOL'], '{path?}', [WebDavController::class, 'mkcol'])
    ->where('path', '.*')
    ->middleware('web');
