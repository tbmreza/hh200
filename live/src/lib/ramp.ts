import type { Request } from './types';

// A single point on a concurrency-over-time curve: `active` VUs are online
// starting at time `t` (seconds) until the next point.
export type ConcurrencyPoint = { t: number; active: number };

// Seconds before VU `i` (0-based) starts, given a total `rampUp` window over
// `nvu` VUs. Mirrors the backend's `staggerMicros` (rampUp/(nvu-1) per VU).
export function staggerSeconds(rampUp: number, nvu: number, i: number): number {
	if (rampUp <= 0 || nvu <= 1) return 0;
	return (rampUp * i) / (nvu - 1);
}

// Planned active VUs over time as a step series. `duration` = 0 is a single-shot
// (no hold); otherwise VUs hold at `nvu` until `duration`, then drop to 0.
export function rampCurve(nvu: number, rampUp: number, duration: number): ConcurrencyPoint[] {
	const n = Math.max(0, Math.floor(Number.isFinite(nvu) ? nvu : 0));
	if (n === 0) return [];
	const ramp = Math.max(0, Number.isFinite(rampUp) ? rampUp : 0);
	const dur = Math.max(0, Number.isFinite(duration) ? duration : 0);

	const points: ConcurrencyPoint[] = [];
	for (let i = 0; i < n; i++) {
		const t = staggerSeconds(ramp, n, i);
		if (dur > 0 && t > dur) break; // the run ends before this VU would start
		points.push({ t, active: i + 1 });
	}

	if (dur <= 0) {
		const last = points[points.length - 1]?.t ?? 0;
		points.push({ t: last, active: 0 });
	} else {
		points.push({ t: dur, active: 0 });
	}
	return points;
}

// Observed concurrency: distinct active workers per time bucket, derived from
// captured requests. `sent_at` is whole seconds, so resolution is one second.
export function observedConcurrency(requests: Request[], bucketSec = 1): ConcurrencyPoint[] {
	const withWorker = requests.filter((r) => r.worker_id != null);
	if (withWorker.length === 0) return [];
	const min = Math.floor(Math.min(...withWorker.map((r) => r.sent_at)));
	const max = Math.floor(Math.max(...withWorker.map((r) => r.sent_at)));

	const points: ConcurrencyPoint[] = [];
	for (let t = min; t <= max; t += bucketSec) {
		const active = new Set<number>();
		for (const r of withWorker) {
			if (r.worker_id != null && r.sent_at >= t && r.sent_at < t + bucketSec) {
				active.add(r.worker_id);
			}
		}
		points.push({ t, active: active.size });
	}
	return points;
}
