<?php

namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Request;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;
use Fpdf\Fpdf;

class PdfController extends AbstractController
{
    #[Route('/mkpdf', name: 'app_pdf')]
    public function index(Request $request): Response
    {
        $data = json_decode($request->getContent(), true);

        if (!$data || empty($data['title']) || empty($data['content'])) {
            return $this->json(['error' => 'Bad Request: "title" and "content" are required fields.'], 400);
        }

        $title = $data['title'];
        $content = $data['content'];

        $pdf = new Fpdf();
        $pdf->AddPage();
        $pdf->SetFont('Arial', 'B', 16);
        $pdf->Cell(0, 10, utf8_decode($title), 0, 1, 'C');
        $pdf->Ln(10);
        $pdf->SetFont('Arial', '', 12);
        $pdf->MultiCell(0, 10, utf8_decode($content));

        $response = new Response($pdf->Output('S'));
        $response->headers->set('Content-Type', 'application/pdf');
        $response->headers->set('Content-Disposition', 'attachment; filename="document.pdf"');
        $response->headers->set('Access-Control-Allow-Origin', '*');
        $response->headers->set('Access-Control-Allow-Methods', 'POST, OPTIONS');
        $response->headers->set('Access-Control-Allow-Headers', 'Content-Type');


        return $response;
    }
}
