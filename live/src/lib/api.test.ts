import { describe, expect, test } from 'bun:test';
import type { Run, RunDetail } from '$lib/types';
import { listRuns, getRun, getReportUrl, getReportFilename, sendSignal } from './api';

function jsonFetch(body: unknown): typeof fetch {
	return (async () => new Response(JSON.stringify(body))) as unknown as typeof fetch;
}

const run: Run = {
	id: 1,
	name: 'a',
	status: 'running',
	script_path: '/s',
	concurrency: 1,
	rate_limit: 1,
	started_at: 0,
	ended_at: null,
	control_socket: '/tmp/x',
};

describe('api', () => {
	test('listRuns returns runs', async () => {
		const result = await listRuns(jsonFetch({ runs: [run] }));
		expect(result).toEqual([run]);
	});

	test('getRun returns the run detail', async () => {
		const detail: RunDetail = { run, requests: [], metric_windows: [] };
		const result = await getRun(1, jsonFetch(detail));
		expect(result).toEqual(detail);
	});

	test('report helpers build the CSV url and filename', () => {
		expect(getReportUrl(42)).toBe('/api/report/42');
		expect(getReportFilename(42)).toBe('stats_history_42.csv');
	});

	test('sendSignal posts the raw signal body', async () => {
		const captured: { url: string; method: string; body: string }[] = [];
		const fetchImpl = (async (url: string, init?: RequestInit) => {
			captured.push({ url, method: init?.method ?? 'GET', body: String(init?.body) });
			return new Response('{}');
		}) as unknown as typeof fetch;

		await sendSignal('pause', fetchImpl);
		expect(captured).toEqual([{ url: '/api/sig', method: 'POST', body: 'pause' }]);
	});
});
