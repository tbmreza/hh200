import type { LoadEvent } from '@sveltejs/kit';
import { features } from '$lib/features';

export const prerender = false;

export function load({ params }: LoadEvent) {
  return { implemented: features.runDetail, id: params.id };
}
