import { error } from '@sveltejs/kit';
import type { LoadEvent } from '@sveltejs/kit';
import { getRun } from '$lib/api';

export const prerender = false;

export async function load({ fetch, params }: LoadEvent) {
	const id = Number(params.id);
	if (!Number.isInteger(id) || id <= 0) throw error(404, `Run not found: ${params.id}`);
	const detail = await getRun(id, fetch);
	return { detail };
}
