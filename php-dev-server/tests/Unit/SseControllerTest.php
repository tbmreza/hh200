<?php

namespace Tests\Unit;

use App\Http\Controllers\SseController;
use DateTimeImmutable;
use PHPUnit\Framework\TestCase;

class SseControllerTest extends TestCase
{
    private SseController $controller;

    protected function setUp(): void
    {
        parent::setUp();

        $this->controller = new SseController();
    }

    public function test_parse_params_returns_defaults_when_input_is_empty(): void
    {
        $this->assertSame([0, 1], $this->controller->parseParams([]));
    }

    public function test_parse_params_reads_events_and_interval(): void
    {
        $this->assertSame([5, 2], $this->controller->parseParams(['events' => '5', 'interval' => '2']));
    }

    public function test_parse_params_clamps_negative_values(): void
    {
        $this->assertSame([0, 1], $this->controller->parseParams(['events' => '-3', 'interval' => '-1']));
    }

    public function test_message_block_format(): void
    {
        $time = new DateTimeImmutable('2026-01-01T00:00:00+00:00');
        $expected = "id: 3\nevent: message\ndata: {\"id\":3,\"time\":\"2026-01-01T00:00:00+00:00\",\"message\":\"Event #3\"}\n\n";

        $this->assertSame($expected, $this->controller->messageBlock(3, $time));
    }

    public function test_done_block_format(): void
    {
        $expected = "event: done\ndata: {\"total\":3}\n\n";

        $this->assertSame($expected, $this->controller->doneBlock(3));
    }
}
