import type { LoadEvent } from '@sveltejs/kit';
import type { Run, RunsResponse } from '$lib/types';

export const prerender = false;

export async function load({ fetch }: LoadEvent) {
	const res = await fetch('/api/runs');
	const json = (await res.json()) as RunsResponse;
	const chartData = json.runs.map((run: Run) => ({
		year: run.name.length,
		count: run.script_path.length
	}));
	return { chartData, runs: json.runs };
}
