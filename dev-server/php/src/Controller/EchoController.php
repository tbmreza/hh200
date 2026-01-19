<?php

namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class EchoController extends AbstractController
{
    #[Route('/echo', name: 'app_echo')]
    public function index(Request $request): Response
    {
        $output = "=== PHP ECHO SERVER ===\n\n";
        $output .= "METHOD: " . $request->getMethod() . "\n";
        $output .= "URI: " . $request->getRequestUri() . "\n";
        $output .= "PROTOCOL: " . $request->server->get('SERVER_PROTOCOL') . "\n\n";

        $output .= "=== HEADERS ===\n";
        foreach ($request->headers->all() as $name => $values) {
            $output .= "$name: " . implode(', ', $values) . "\n";
        }
        $output .= "\n";

        if ($request->query->count() > 0) {
            $output .= "=== QUERY PARAMETERS ===\n";
            foreach ($request->query->all() as $key => $value) {
                if (is_array($value)) {
                    $output .= "$key: " . implode(', ', $value) . "\n";
                } else {
                    $output .= "$key: $value\n";
                }
            }
            $output .= "\n";
        }

        if ($request->request->count() > 0) {
            $output .= "=== FORM DATA ===\n";
            foreach ($request->request->all() as $key => $value) {
                if (is_array($value)) {
                    $output .= "$key: " . implode(', ', $value) . "\n";
                } else {
                    $output .= "$key: $value\n";
                }
            }
            $output .= "\n";
        }

        $body = $request->getContent();
        if (!empty($body)) {
            $output .= "=== RAW REQUEST BODY ===\n";
            $output .= "Length: " . strlen($body) . " bytes\n";
            $output .= "Content:\n";
            $output .= $body . "\n\n";
        }

        if ($request->files->count() > 0) {
            $output .= "=== UPLOADED FILES ===\n";
            foreach ($request->files->all() as $key => $file) {
                $output .= "Field: $key\n";
                $output .= "  Name: " . $file->getClientOriginalName() . "\n";
                $output .= "  Type: " . $file->getClientMimeType() . "\n";
                $output .= "  Size: " . $file->getSize() . " bytes\n";
                $output .= "  Tmp: " . $file->getPathname() . "\n";
                $output .= "  Error: " . $file->getError() . "\n\n";
            }
        }

        $output .= "=== SERVER INFO ===\n";
        $output .= "Server Software: " . $request->server->get('SERVER_SOFTWARE', 'Unknown') . "\n";
        $output .= "Server Name: " . $request->server->get('SERVER_NAME') . "\n";
        $output .= "Server Port: " . $request->server->get('SERVER_PORT') . "\n";
        $output .= "Remote Address: " . $request->getClientIp() . "\n";
        $output .= "Remote Port: " . $request->server->get('REMOTE_PORT', 'Unknown') . "\n";
        $output .= "Request Time: " . date('Y-m-d H:i:s', $request->server->get('REQUEST_TIME')) . "\n\n";

        $output .= "=== ADDITIONAL INFO ===\n";
        $output .= "Script Name: " . $request->server->get('SCRIPT_NAME') . "\n";
        $output .= "Document Root: " . $request->server->get('DOCUMENT_ROOT') . "\n";
        $output .= "HTTPS: " . ($request->isSecure() ? 'Yes' : 'No') . "\n";

        if ($request->headers->has('user-agent')) {
            $output .= "User Agent: " . $request->headers->get('user-agent') . "\n";
        }

        $output .= "\n=== END ===\n";

        $response = new Response($output);
        $response->headers->set('Content-Type', 'text/plain');
        $response->headers->set('Access-Control-Allow-Origin', '*');
        $response->headers->set('Access-Control-Allow-Methods', 'GET, POST, PUT, DELETE, PATCH, OPTIONS');
        $response->headers->set('Access-Control-Allow-Headers', '*');

        return $response;
    }
}
