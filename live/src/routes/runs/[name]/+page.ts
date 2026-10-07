import { error } from '@sveltejs/kit';
import type { LoadEvent } from '@sveltejs/kit';
import type { RunsResponse } from '$lib/types';

export const prerender = false;

export async function load({ fetch, params }: LoadEvent) {
	const res = await fetch('/api/runs');
	if (!res.ok) throw new Error(`/api/runs returned HTTP ${res.status}`);
	const json = (await res.json()) as RunsResponse;
	const run = json.runs.find((r) => r.name === params.name);
	if (!run) throw error(404, `Run not found: ${params.name}`);
	return { run };
}
