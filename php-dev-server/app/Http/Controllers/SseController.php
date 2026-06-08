<?php

namespace App\Http\Controllers;

use Symfony\Component\HttpFoundation\StreamedResponse;

class SseController extends Controller
{
    public function __invoke()
    {
        $response = new StreamedResponse(function () {
            $maxEvents = (int) request('events', 0);
            $interval = (int) request('interval', 1);
            $count = 0;

            set_time_limit(0);

            while (true) {
                if (connection_aborted()) {
                    break;
                }

                $count++;

                $data = json_encode([
                    'id' => $count,
                    'time' => now()->toIso8601String(),
                    'message' => "Event #{$count}",
                ]);

                echo "id: {$count}\n";
                echo "event: message\n";
                echo "data: {$data}\n\n";

                ob_flush();
                flush();

                if ($maxEvents > 0 && $count >= $maxEvents) {
                    $done = json_encode(['total' => $count]);
                    echo "event: done\n";
                    echo "data: {$done}\n\n";
                    ob_flush();
                    flush();
                    break;
                }

                sleep($interval);
            }
        });

        $response->headers->set('Content-Type', 'text/event-stream');
        $response->headers->set('Cache-Control', 'no-cache');
        $response->headers->set('Connection', 'keep-alive');
        $response->headers->set('X-Accel-Buffering', 'no');

        return $response;
    }
}
