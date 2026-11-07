import type { Request, RequestBody, RequestHeader, RunDetail } from './types';

// Escape text for safe interpolation into an HTML document. URLs, headers, and
// bodies can all contain markup characters.
export function escapeHtml(value: string): string {
	return value
		.replaceAll('&', '&amp;')
		.replaceAll('<', '&lt;')
		.replaceAll('>', '&gt;')
		.replaceAll('"', '&quot;')
		.replaceAll("'", '&#39;');
}

// Decode a `content` field. The backend (and the Prisma seed) store request/
// response bodies as `Bytes`, which serialize to a base64 string over JSON
// (see types.ts). Best-effort: falls back to the raw value when it isn't
// valid base64.
export function decodeContent(content: string | null): string {
	if (!content) return '';
	try {
		const binary = atob(content);
		const bytes = Uint8Array.from(binary, (ch) => ch.charCodeAt(0));
		return new TextDecoder('utf-8', { fatal: false }).decode(bytes);
	} catch {
		return content;
	}
}

function fmtTime(secs: number | null): string {
	return secs == null ? 'N/A' : new Date(secs * 1000).toLocaleString();
}

function renderHeaders(headers: RequestHeader[]): string {
	if (headers.length === 0) return '';
	const items = headers
		.map(
			(h) =>
				`<div class="header"><span class="dir">${escapeHtml(h.direction)}</span><strong>${escapeHtml(h.name)}:</strong> ${escapeHtml(h.value)}</div>`
		)
		.join('\n');
	return `<div class="headers">${items}</div>`;
}

function renderBodies(bodies: RequestBody[]): string {
	if (bodies.length === 0) return '';
	const items = bodies
		.map((b) => {
			const mark = b.truncated ? ' <em>(truncated)</em>' : '';
			return `<div class="body"><span class="dir">${escapeHtml(b.direction)} body</span>${mark}<pre>${escapeHtml(decodeContent(b.content))}</pre></div>`;
		})
		.join('\n');
	return `<div class="bodies">${items}</div>`;
}

function renderRequest(req: Request): string {
	const error = req.error ? `<p class="error">error: ${escapeHtml(req.error)}</p>` : '';
	return `<details class="request">
  <summary>
    <span class="seq">#${req.seq}</span>
    <span class="method">${escapeHtml(req.method)}</span>
    <span class="url">${escapeHtml(req.url)}</span>
    <span class="status">${req.status_code ?? '—'}</span>
    <span class="dur">${req.duration_ms ?? '—'} ms</span>
  </summary>
  ${error}
  ${renderHeaders(req.headers)}
  ${renderBodies(req.bodies)}
</details>`;
}

const meta = (label: string, value: string) => `<dt>${escapeHtml(label)}</dt><dd>${value}</dd>`;

// Render a run as a single self-contained HTML document (inline styles, no
// external assets) suitable for download.
export function runDetailToHtml(detail: RunDetail): string {
	const { run, requests } = detail;
	const rows = requests.map(renderRequest).join('\n');

	return `<!doctype html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>${escapeHtml(run.name)} — Run detail</title>
<style>
:root { color-scheme: light dark; }
* { box-sizing: border-box; }
body { font-family: system-ui, -apple-system, "Segoe UI", Roboto, sans-serif; margin: 2rem auto; max-width: 60rem; padding: 0 1rem; color: #1f2937; }
header.page { display: flex; align-items: baseline; gap: 1rem; border-bottom: 1px solid #e5e7eb; padding-bottom: .75rem; }
h1 { margin: 0; font-size: 1.75rem; }
.badge { padding: .1rem .5rem; border-radius: 999px; font-size: .85em; }
.badge.running { background: #fff7e6; color: #b45309; }
.badge.completed { background: #ecfdf5; color: #047857; }
.badge.failed { background: #fef2f2; color: #b91c1c; }
.badge.cancelled { background: #f3f4f6; color: #4b5563; }
dl.meta { display: grid; grid-template-columns: max-content 1fr; gap: .25rem 1rem; margin: 1rem 0; }
dt { color: #6b7280; }
dd { margin: 0; }
details.request { border: 1px solid #e5e7eb; border-radius: .5rem; margin: .5rem 0; padding: .5rem .75rem; }
summary { cursor: pointer; display: flex; flex-wrap: wrap; gap: 1rem; font-family: monospace; }
.method { font-weight: 700; }
.url { color: #2563eb; overflow-wrap: anywhere; }
.seq { color: #6b7280; }
.status { font-weight: 600; }
.dir { display: inline-block; margin-right: .5rem; text-transform: uppercase; font-size: .8em; color: #6b7280; }
.header, .body { font-family: monospace; margin: .25rem 0; }
pre { white-space: pre-wrap; word-break: break-all; margin: .25rem 0 0; padding: .5rem; background: #f9fafb; border-radius: .25rem; }
.error { color: #b91c1c; font-family: monospace; }
@media (prefers-color-scheme: dark) { body { color: #e5e7eb; } pre { background: #1f2937; } details.request { border-color: #374151; } }
</style>
</head>
<body>
<header class="page">
  <h1>${escapeHtml(run.name)}</h1>
  <span class="badge ${escapeHtml(run.status)}">${escapeHtml(run.status)}</span>
</header>
<dl class="meta">
  ${meta('Script path', escapeHtml(run.script_path))}
  ${meta('Started at', fmtTime(run.started_at))}
  ${meta('Ended at', fmtTime(run.ended_at))}
  ${meta('Concurrency', String(run.concurrency))}
  ${meta('Rate limit', String(run.rate_limit))}
  ${meta('Control socket', escapeHtml(run.control_socket))}
</dl>
<h2>Requests</h2>
${requests.length ? rows : '<p>No requests recorded.</p>'}
</body>
</html>`;
}

// Build the HTML and trigger a client-side download.
export function downloadRunDetail(detail: RunDetail, filename = 'run.html'): void {
	const html = runDetailToHtml(detail);
	const blob = new Blob([html], { type: 'text/html' });
	const url = URL.createObjectURL(blob);
	const a = document.createElement('a');
	a.href = url;
	a.download = filename;
	document.body.appendChild(a);
	a.click();
	a.remove();
	URL.revokeObjectURL(url);
}
