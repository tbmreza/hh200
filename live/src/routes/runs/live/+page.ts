import type { LoadEvent } from '@sveltejs/kit';
import { getLiveRun } from '$lib/api';

export const prerender = false;

export async function load({ fetch }: LoadEvent) {
	const live = await getLiveRun(fetch);
	return { live };
}
