import { describe, expect, test } from 'bun:test';
import { makeLoad } from '$lib/test-utils';
import { load } from './+page';

const live = {
	run: {
		id: 7,
		name: 'live-run',
		status: 'running',
		script_path: '/scripts/live.hhs',
		concurrency: 3,
		rate_limit: 10,
		started_at: 0,
		ended_at: null,
		control_socket: '/tmp/live.sock',
	},
	state: 'running',
};

describe('live page load', () => {
	test('returns the in-progress run and control state', async () => {
		const data = await load(makeLoad(JSON.stringify(live)));
		expect(data.live.run.name).toBe('live-run');
		expect(data.live.state).toBe('running');
	});
});
