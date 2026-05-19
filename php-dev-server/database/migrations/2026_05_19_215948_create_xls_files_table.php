<?php

use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Support\Facades\Schema;

        return new class extends Migration
        {
            /**
             * Run the migrations.
             */
            public function up(): void
            {
                Schema::create('xls_files', function (Blueprint $table) {
                    $table->id();
                    $table->string('filename'); // stored filename
                    $table->string('original_name'); // original uploaded name
                    $table->string('path'); // storage path
                    $table->unsignedBigInteger('size')->nullable(); // file size in bytes
                    $table->timestamps();
                });
            }

            /**
             * Reverse the migrations.
             */
            public function down(): void
            {
                Schema::dropIfExists('xls_files');
            }
        };
