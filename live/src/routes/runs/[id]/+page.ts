import type { LoadEvent } from '@sveltejs/kit';
import type { RunsResponse } from '$lib/types';

export const prerender = false;

export async function load({ fetch, params }: LoadEvent) {
	const res = await fetch('/api/runs');
	const json = (await res.json()) as RunsResponse;
	const run = json.runs.find(r => r.name === params.id);
	return { run };
}
