<?php

namespace App\Http\Controllers;

use DateTimeInterface;
use Symfony\Component\HttpFoundation\StreamedResponse;

class SseController extends Controller
{
    public function __invoke()
    {
        $response = new StreamedResponse(function () {
            $this->stream();
        });

        $response->headers->set('Content-Type', 'text/event-stream');
        $response->headers->set('Cache-Control', 'no-cache');
        $response->headers->set('Connection', 'keep-alive');
        $response->headers->set('X-Accel-Buffering', 'no');

        return $response;
    }

    public function stream(): void
    {
        [$maxEvents, $interval] = $this->parseParams(request()->all());
        $count = 0;

        set_time_limit(0);

        while (true) {
            if (connection_aborted()) {
                break;
            }

            $count++;

            $this->write($this->messageBlock($count, now()));

            if ($maxEvents > 0 && $count >= $maxEvents) {
                $this->write($this->doneBlock($count));
                break;
            }

            sleep($interval);
        }
    }

    public function parseParams(array $input): array
    {
        $maxEvents = max(0, (int) ($input['events'] ?? 0));
        $interval = max(1, (int) ($input['interval'] ?? 1));

        return [$maxEvents, $interval];
    }

    public function messageBlock(int $id, DateTimeInterface $time): string
    {
        $data = json_encode([
            'id' => $id,
            'time' => $time->format(DateTimeInterface::ATOM),
            'message' => "Event #{$id}",
        ]);

        return "id: {$id}\nevent: message\ndata: {$data}\n\n";
    }

    public function doneBlock(int $total): string
    {
        $data = json_encode(['total' => $total]);

        return "event: done\ndata: {$data}\n\n";
    }

    public function write(string $block): void
    {
        echo $block;
        ob_flush();
        flush();
    }
}
