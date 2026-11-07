import { describe, expect, test } from 'bun:test';
import type { RunDetail } from '$lib/types';
import { decodeContent, escapeHtml, runDetailToHtml } from './export';

const detail: RunDetail = {
	run: {
		id: 1,
		name: 'alpha & <run>',
		status: 'completed',
		script_path: '/scripts/a.hhs',
		concurrency: 2,
		rate_limit: 10,
		started_at: 0,
		ended_at: null,
		control_socket: '/tmp/a.sock',
	},
	requests: [
		{
			id: 1,
			seq: 0,
			sent_at: 0,
			duration_ms: 12.5,
			method: 'POST',
			url: 'http://example.com/<script>',
			status_code: 200,
			error: null,
			bytes_in: 100,
			bytes_out: 50,
			worker_id: 0,
			headers: [{ id: 1, direction: 'response', name: 'Content-Type', value: 'text/plain' }],
			bodies: [{ id: 1, direction: 'response', content: 'aGVsbG8=', truncated: false }],
		},
	],
};

describe('escapeHtml', () => {
	test('escapes markup characters', () => {
		expect(escapeHtml('<a href="x">&\'')).toBe('&lt;a href=&quot;x&quot;&gt;&amp;&#39;');
	});
});

describe('decodeContent', () => {
	test('decodes base64 as UTF-8', () => {
		expect(decodeContent('aGVsbG8=')).toBe('hello');
		expect(decodeContent('Y2Fmw6k=')).toBe('café');
	});

	test('returns empty for null/empty', () => {
		expect(decodeContent(null)).toBe('');
		expect(decodeContent('')).toBe('');
	});

	test('falls back to the raw value on invalid base64', () => {
		expect(decodeContent('not base64!!')).toBe('not base64!!');
	});
});

describe('runDetailToHtml', () => {
	test('is a self-contained document', () => {
		const html = runDetailToHtml(detail);
		expect(html).toContain('<!doctype html>');
		expect(html).toContain('<style>');
	});

	test('escapes the run name and request url', () => {
		const html = runDetailToHtml(detail);
		expect(html).toContain('alpha &amp; &lt;run&gt;');
		expect(html).toContain('http://example.com/&lt;script&gt;');
		expect(html).not.toContain('http://example.com/<script>');
	});

	test('decodes body content', () => {
		const html = runDetailToHtml(detail);
		expect(html).toContain('hello');
	});

	test('renders request summary fields', () => {
		const html = runDetailToHtml(detail);
		expect(html).toContain('#0');
		expect(html).toContain('POST');
		expect(html).toContain('200');
	});
});
