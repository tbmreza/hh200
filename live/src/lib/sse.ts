import { writable } from 'svelte/store';
import type { Run, RunControlState, SseEvent } from '$lib/types';

export type LiveState = {
	run: Run | null;
	state: RunControlState;
};

export const live = writable<LiveState>({ run: null, state: 'running' });

export function setLiveRun(run: Run | null, state: RunControlState = 'running'): void {
	live.set({ run, state });
}

// Opens the SSE stream and applies parsed events to the `live` store.
// Returns a teardown function that closes the connection.
export function connectSse(url = '/api/sse'): () => void {
	const es = new EventSource(url);

	es.onmessage = (event) => {
		const parsed = parseSse(event.data as string);
		if (parsed) apply(parsed);
	};

	return () => es.close();
}

// Parses a raw SSE `data:` payload into a typed event, or `null` when the
// frame is unknown (e.g. the backend's current placeholder payload).
export function parseSse(data: string): SseEvent | null {
	let obj: unknown;
	try {
		obj = JSON.parse(data);
	} catch {
		return null;
	}
	if (typeof obj !== 'object' || obj === null) return null;
	const record = obj as Record<string, unknown>;

	const kind = record.kind ?? record.type;
	if (kind === 'NewRow' || kind === 'new_row' || kind === 'newRow') {
		if (isRun(record.run)) return { kind: 'NewRow', run: record.run };
	}
	if (kind === 'UpdateRow' || kind === 'update_row' || kind === 'updateRow') {
		const patch = record.patch ?? record;
		if (typeof record.id === 'number') {
			return { kind: 'UpdateRow', id: record.id, patch: patch as Partial<Run> };
		}
	}
	if (kind === 'NoNews' || kind === 'no_news' || kind === 'noNews') {
		return { kind: 'NoNews' };
	}

	// Fallback: a bare run object is treated as a new row.
	const run = record.run ?? (isRun(record) ? record : null);
	if (isRun(run)) return { kind: 'NewRow', run };

	return null;
}

function apply(event: SseEvent): void {
	live.update((state) => {
		switch (event.kind) {
			case 'NoNews':
				return state;
			case 'NewRow':
				return { run: event.run, state: 'running' };
			case 'UpdateRow':
				if (state.run && state.run.id === event.id) {
					return { ...state, run: { ...state.run, ...event.patch } };
				}
				return state;
		}
	});
}

function isRun(value: unknown): value is Run {
	if (typeof value !== 'object' || value === null) return false;
	const r = value as Record<string, unknown>;
	return typeof r.id === 'number' && typeof r.name === 'string';
}
