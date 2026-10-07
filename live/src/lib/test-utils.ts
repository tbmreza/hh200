import type { LoadEvent } from '@sveltejs/kit';

// A `fetch` stub that always resolves to a 200 Response with `json` as its body.
export function makeFetch(json: string): LoadEvent['fetch'] {
	const stub = async () => new Response(json);
	return stub as unknown as LoadEvent['fetch'];
}

export function makeLoad(json: string, params: Record<string, string> = {}): LoadEvent {
	const event = { fetch: makeFetch(json), params };
	return event as unknown as LoadEvent;
}
