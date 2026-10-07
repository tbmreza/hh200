import { describe, expect, test } from 'bun:test';
import type { RunsResponse } from '$lib/types';
import { makeLoad } from '$lib/test-utils';
import { load } from './+page';

const runs: RunsResponse['runs'] = [
	{ id: 1, name: 'alpha-run', status: 'running', script_path: '/scripts/a.hhs', concurrency: 2, rate_limit: 10, started_at: 0, ended_at: null, control_socket: '/tmp/a.sock' },
	{ id: 2, name: 'beta-run', status: 'completed', script_path: '/scripts/b.hhs', concurrency: 5, rate_limit: 20, started_at: 1, ended_at: 2, control_socket: '/tmp/b.sock' },
];

describe('run detail page load', () => {
	test('finds the run matching params.name', async () => {
		const data = await load(makeLoad(JSON.stringify({ runs }), { name: 'beta-run' }));
		expect(data.run.name).toBe('beta-run');
	});

	test('throws for an unknown name', async () => {
		await expect(load(makeLoad(JSON.stringify({ runs }), { name: 'nope-run' }))).rejects.toThrow();
	});

	test('throws when the API has no runs', async () => {
		await expect(load(makeLoad(JSON.stringify({ runs: [] }), { name: 'alpha-run' }))).rejects.toThrow();
	});
});
