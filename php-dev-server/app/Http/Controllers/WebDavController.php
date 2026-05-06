<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Illuminate\Http\Response;
use Illuminate\Support\Facades\Storage;
use Symfony\Component\HttpFoundation\StreamedResponse;

class WebDavController extends Controller
{
    private string $basePath = '';

    public function __construct()
    {
        $this->basePath = 'webdav';
    }

    public function propfind(Request $request, string $path = ''): Response|StreamedResponse
    {
        $depth = $request->header('Depth', '1');
        $fullPath = $this->getFullPath($path);

        if (!Storage::exists($fullPath) && !empty($path)) {
            return response('Not Found', 404);
        }

        $xml = $this->buildPropfindResponse($fullPath, $depth, $path);

        return response($xml, 207, [
            'Content-Type' => 'application/xml; charset=utf-8',
            'DAV' => '1',
        ]);
    }

    public function mkcol(Request $request, string $path = ''): Response
    {
        $fullPath = $this->getFullPath($path);

        if (Storage::exists($fullPath)) {
            return response('Method Not Allowed', 405);
        }

        if (Storage::makeDirectory($fullPath)) {
            return response('', 201);
        }

        return response('Conflict', 409);
    }

    private function getFullPath(string $path): string
    {
        $path = trim($path, '/');
        return empty($path) ? $this->basePath : $this->basePath . '/' . $path;
    }

    private function buildPropfindResponse(string $path, string $depth, string $requestPath): string
    {
        $items = $this->getDirectoryContents($path);

        $xml = '<?xml version="1.0" encoding="utf-8"?>';
        $xml .= '<d:multistatus xmlns:d="DAV:" xmlns:oc="http://owncloud.org/ns">';

        foreach ($items as $item) {
            $itemPath = ltrim(str_replace($this->basePath, '', $item['path']), '/');
            $href = '/' . (empty($itemPath) ? '' : $itemPath);

            if (empty($requestPath) && $itemPath !== '' && $depth === '0') {
                continue;
            }

            $xml .= '<d:response>';
            $xml .= '<d:href>' . htmlspecialchars($href) . '</d:href>';
            $xml .= '<d:propstat>';
            $xml .= '<d:prop>';
            $xml .= '<d:resourcetype>';
            if ($item['type'] === 'dir') {
                $xml .= '<d:collection/>';
            }
            $xml .= '</d:resourcetype>';
            $xml .= '<d:getcontentlength>' . ($item['size'] ?? 0) . '</d:getcontentlength>';
            $xml .= '<d:getlastmodified>' . gmdate('D, d M Y H:i:s', $item['timestamp']) . ' GMT</d:getlastmodified>';
            $xml .= '<d:displayname>' . htmlspecialchars($item['name']) . '</d:displayname>';
            $xml .= '</d:prop>';
            $xml .= '<d:status>HTTP/1.1 200 OK</d:status>';
            $xml .= '</d:propstat>';
            $xml .= '</d:response>';
        }

        $xml .= '</d:multistatus>';

        return $xml;
    }

    private function getDirectoryContents(string $path): array
    {
        $items = [];

        if (!Storage::exists($path)) {
            return $items;
        }

        $contents = Storage::directories($path);
        foreach ($contents as $dir) {
            $items[] = [
                'path' => $dir,
                'name' => basename($dir),
                'type' => 'dir',
                'size' => 0,
                'timestamp' => Storage::lastModified($dir) ?? time(),
            ];
        }

        $files = Storage::files($path);
        foreach ($files as $file) {
            $items[] = [
                'path' => $file,
                'name' => basename($file),
                'type' => 'file',
                'size' => Storage::size($file),
                'timestamp' => Storage::lastModified($file) ?? time(),
            ];
        }

        return $items;
    }
}