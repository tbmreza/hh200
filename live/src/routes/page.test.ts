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

function makeLoad(runs: RunsResponse['runs']): LoadEvent {
  const event = {
    fetch: makeFetch(JSON.stringify({ runs })),
  };
  return event as unknown as LoadEvent;
}

describe('layout page load', () => {
  test('maps chartData from run names and script paths', async () => {
    const data = await load(makeLoad([
      { id: 1, name: 'abcd', status: 'running', script_path: '/scripts/x.hhs', concurrency: 2, rate_limit: 10, started_at: 0, ended_at: null, control_socket: '/tmp/a.sock' },
      { id: 2, name: 'ab', status: 'completed', script_path: '/s/y.hhs', concurrency: 5, rate_limit: 20, started_at: 1, ended_at: 2, control_socket: '/tmp/b.sock' },
    ]));

    expect(data.chartData).toEqual([
      { year: 4, count: 14 },
      { year: 2, count: 8 },
    ]);
  });

  test('returns empty runs for an empty payload', async () => {
    const data = await load(makeLoad([]));
    expect(data.runs).toEqual([]);
    expect(data.chartData).toEqual([]);
  });
});