import { describe, expect, test } from 'bun:test';
import type { Run, RunDetail, RunPlan } from '$lib/types';
import { listRuns, getRun, sendSignal, planRun, startRun } from './api';

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
		const detail: RunDetail = { run, requests: [] };
		const result = await getRun(1, jsonFetch(detail));
		expect(result).toEqual(detail);
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

	test('planRun posts the request and returns the plan', async () => {
		const plan: RunPlan = {
			scriptValid: true,
			errors: [],
			inFlightCeiling: 1,
			wallClock: 'single-shot',
		};
		const captured: { url: string; method: string; body: string }[] = [];
		const fetchImpl = (async (url: string, init?: RequestInit) => {
			captured.push({ url, method: init?.method ?? 'GET', body: String(init?.body) });
			return new Response(JSON.stringify(plan));
		}) as unknown as typeof fetch;

		const req = { name: 'a', script: '/s.hhs', nvu: 1, duration: 0, rampUp: 0 };
		const result = await planRun(req, fetchImpl);
		expect(result).toEqual(plan);
		expect(captured).toEqual([
			{ url: '/api/run/plan', method: 'POST', body: JSON.stringify(req) },
		]);
	});

	test('startRun posts the request and returns the run id', async () => {
		const captured: { url: string; method: string; body: string }[] = [];
		const fetchImpl = (async (url: string, init?: RequestInit) => {
			captured.push({ url, method: init?.method ?? 'GET', body: String(init?.body) });
			return new Response(JSON.stringify({ id: 7 }));
		}) as unknown as typeof fetch;

		const req = { name: 'a', script: '/s.hhs', nvu: 1, duration: 0, rampUp: 0 };
		const result = await startRun(req, fetchImpl);
		expect(result).toEqual({ id: 7 });
		expect(captured).toEqual([{ url: '/api/run', method: 'POST', body: JSON.stringify(req) }]);
	});
});
