import type {
	LiveRun,
	Run,
	RunDetail,
	RunSignal,
	RunsResponse,
} from '$lib/types';

type FetchLike = typeof fetch;

async function asJson<T>(res: Response): Promise<T> {
	if (!res.ok) throw new Error(`request failed: ${res.url} returned HTTP ${res.status}`);
	return res.json() as Promise<T>;
}

export async function listRuns(fetchImpl: FetchLike = fetch): Promise<Run[]> {
	const res = await fetchImpl('/api/runs');
	const json = await asJson<RunsResponse>(res);
	return json.runs;
}

export async function getRun(id: number, fetchImpl: FetchLike = fetch): Promise<RunDetail> {
	const res = await fetchImpl(`/api/runs/${id}`);
	return asJson<RunDetail>(res);
}

export async function getLiveRun(fetchImpl: FetchLike = fetch): Promise<LiveRun> {
	const res = await fetchImpl('/api/runs/live');
	return asJson<LiveRun>(res);
}

export function getReportUrl(id: number): string {
	return `/api/report/${id}`;
}

export function getReportFilename(id: number): string {
	return `stats_history_${id}.csv`;
}

export async function sendSignal(signal: RunSignal, fetchImpl: FetchLike = fetch): Promise<void> {
	const res = await fetchImpl('/api/sig', {
		method: 'POST',
		headers: { 'Content-Type': 'text/plain' },
		body: signal,
	});
	if (!res.ok) throw new Error(`signal ${signal} returned HTTP ${res.status}`);
}
