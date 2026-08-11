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

function makeLoad(json: string): LoadEvent {
  const event = {
    fetch: makeFetch(json),
  };
  return event as unknown as LoadEvent;
}

describe('runs page load', () => {
  test('returns all runs from the API', async () => {
    const runs: RunsResponse['runs'] = [
      { id: 1, name: 'alpha-run', status: 'running', script_path: '/scripts/a.hhs', concurrency: 2, rate_limit: 10, started_at: 0, ended_at: null, control_socket: '/tmp/a.sock' },
      { id: 2, name: 'beta-run', status: 'completed', script_path: '/scripts/b.hhs', concurrency: 5, rate_limit: 20, started_at: 1, ended_at: 2, control_socket: '/tmp/b.sock' },
    ];
    const data = await load(makeLoad(JSON.stringify({ runs })));
    expect(data.runs).toHaveLength(2);
    expect(data.runs.map(r => r.name)).toEqual(['alpha-run', 'beta-run']);
  });

  test('returns empty runs for an empty payload', async () => {
    const data = await load(makeLoad(JSON.stringify({ runs: [] })));
    expect(data.runs).toEqual([]);
  });
});