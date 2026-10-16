import { describe, expect, test } from 'bun:test';
import { makeLoad } from '$lib/test-utils';
import { load } from './+page';

const run = {
	id: 1,
	name: 'alpha-run',
	status: 'completed',
	script_path: '/scripts/a.hhs',
	concurrency: 2,
	rate_limit: 10,
	started_at: 0,
	ended_at: 2,
	control_socket: '/tmp/a.sock',
};

const detail = { run, requests: [], metric_windows: [] };

describe('run detail page load', () => {
	test('returns the run detail', async () => {
		const data = await load(makeLoad(JSON.stringify(detail), { id: '1' }));
		expect(data.detail.run.name).toBe('alpha-run');
	});

	test('throws for a non-numeric id', async () => {
		await expect(load(makeLoad(JSON.stringify(detail), { id: 'abc' }))).rejects.toThrow();
	});
});
