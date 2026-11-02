import { describe, expect, test } from 'bun:test';
import type { RunDetail } from '$lib/types';
import { makeLoad } from '$lib/test-utils';
import { load } from './+page';

const detail: RunDetail = {
  run: {
    id: 1,
    name: 'alpha-run',
    status: 'running',
    script_path: '/scripts/a.hhs',
    concurrency: 2,
    rate_limit: 10,
    started_at: 0,
    ended_at: null,
    control_socket: '/tmp/a.sock',
  },
  requests: [],
};

describe('run detail page load', () => {
  test('returns the run detail from the API', async () => {
    const data = await load(makeLoad(JSON.stringify(detail), { id: '1' }));
    expect(data.detail.run.name).toBe('alpha-run');
    expect(data.detail.requests).toEqual([]);
  });
});
