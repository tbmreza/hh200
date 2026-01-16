<?php

namespace Tests\Feature;

use Tests\TestCase;

class ApiTest extends TestCase
{
    public function test_echo_endpoint(): void
    {
        $response = $this->postJson('/api/echo?param=value', [
            'key' => 'data'
        ], ['X-Test-Header' => 'test']);

        $response->assertStatus(200);
        $response->assertJsonPath('body.key', 'data');
        $response->assertJsonPath('params.param', 'value');
    }

    public function test_mkpdf_endpoint(): void
    {
        $response = $this->postJson('/api/mkpdf', [
            'content' => '<h1>Hello World</h1>'
        ]);

        $response->assertStatus(200);
        $response->assertHeader('content-type', 'application/pdf');
    }
}