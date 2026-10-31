import { features } from '$lib/features';

export const prerender = false;

export function load() {
  return { implemented: features.liveRun };
}
