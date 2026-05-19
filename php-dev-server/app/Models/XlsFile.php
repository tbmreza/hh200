<?php

namespace App\Models;

use Illuminate\Database\Eloquent\Model;

class XlsFile extends Model
{
    protected $fillable = [
        'filename',
        'original_name',
        'path',
        'size'
    ];
}
