import type { LoadEvent } from '@sveltejs/kit';
import { listRuns } from '$lib/api';

export const prerender = false;

export async function load({ fetch }: LoadEvent) {
	const runs = await listRuns(fetch);
	return { runs };
}
