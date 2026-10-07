import { describe, expect, test } from 'bun:test';
import type { RunsResponse } from '$lib/types';
import { makeLoad } from '$lib/test-utils';
import { load } from './+page';

const runs: RunsResponse['runs'] = [
	{ id: 1, name: 'abcd', status: 'running', script_path: '/scripts/x.hhs', concurrency: 2, rate_limit: 10, started_at: 0, ended_at: null, control_socket: '/tmp/a.sock' },
	{ id: 2, name: 'ab', status: 'completed', script_path: '/s/y.hhs', concurrency: 5, rate_limit: 20, started_at: 1, ended_at: 2, control_socket: '/tmp/b.sock' },
];

describe('page load', () => {
	test('returns all runs from the API', async () => {
		const data = await load(makeLoad(JSON.stringify({ runs })));
		expect(data.runs).toEqual(runs);
	});

	test('returns empty runs for an empty payload', async () => {
		const data = await load(makeLoad(JSON.stringify({ runs: [] })));
		expect(data.runs).toEqual([]);
	});
});
