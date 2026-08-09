import type { LoadEvent } from '@sveltejs/kit';
import type { RunsResponse } from '$lib/types';

export const prerender = false;

export async function load({ fetch }: LoadEvent) {
	const res = await fetch('/api/runs');
	const json = (await res.json()) as RunsResponse;
	return { runs: json.runs };
}
