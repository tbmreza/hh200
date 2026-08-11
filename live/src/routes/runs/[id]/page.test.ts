import { describe, expect, test } from 'bun:test';
import type { LoadEvent } from '@sveltejs/kit';
import type { RunsResponse } from '$lib/types';
import { load } from './+page';

function makeFetch(json: string): LoadEvent['fetch'] {
  const stub = async (
    _input: Parameters<typeof fetch>[0],
    _init?: Parameters<typeof fetch>[1],
  ) => new Response(json);
  return stub as unknown as LoadEvent['fetch'];
}

function makeLoad(json: string, paramsId: string): LoadEvent {
  const event = {
    params: { id: paramsId },
    fetch: makeFetch(json),
  };
  return event as unknown as LoadEvent;
}

const runs: RunsResponse['runs'] = [
  { id: 1, name: 'alpha-run', status: 'running', script_path: '/scripts/a.hhs', concurrency: 2, rate_limit: 10, started_at: 0, ended_at: null, control_socket: '/tmp/a.sock' },
  { id: 2, name: 'beta-run', status: 'completed', script_path: '/scripts/b.hhs', concurrency: 5, rate_limit: 20, started_at: 1, ended_at: 2, control_socket: '/tmp/b.sock' },
];

describe('run detail page load', () => {
  test('finds the run matching params.id', async () => {
    const data = await load(makeLoad(JSON.stringify({ runs }), 'beta-run'));
    expect(data.run?.name).toBe('beta-run');
  });

  test('returns undefined for an unknown id', async () => {
    const data = await load(makeLoad(JSON.stringify({ runs }), 'nope-run'));
    expect(data.run).toBeUndefined();
  });

  test('returns undefined when the API has no runs', async () => {
    const data = await load(makeLoad(JSON.stringify({ runs: [] }), 'alpha-run'));
    expect(data.run).toBeUndefined();
  });
});