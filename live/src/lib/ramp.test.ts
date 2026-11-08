import { describe, expect, test } from 'bun:test';
import type { Request } from '$lib/types';
import { observedConcurrency, rampCurve, staggerSeconds } from './ramp';

describe('staggerSeconds', () => {
	test('is zero when there is no ramp or a single VU', () => {
		expect(staggerSeconds(0, 10, 5)).toBe(0);
		expect(staggerSeconds(10, 1, 0)).toBe(0);
	});

	test('spreads VUs evenly across the window', () => {
		expect(staggerSeconds(9, 4, 0)).toBe(0);
		expect(staggerSeconds(9, 4, 1)).toBe(3);
		expect(staggerSeconds(9, 4, 3)).toBe(9);
	});
});

describe('rampCurve', () => {
	test('a single VU starts immediately', () => {
		expect(rampCurve(1, 10, 30)).toEqual([
			{ t: 0, active: 1 },
			{ t: 30, active: 0 },
		]);
	});

	test('timed run ramps, then drops at duration', () => {
		const curve = rampCurve(3, 10, 30);
		expect(curve[0]).toEqual({ t: 0, active: 1 });
		expect(curve[1]).toEqual({ t: 5, active: 2 });
		expect(curve[2]).toEqual({ t: 10, active: 3 });
		expect(curve[curve.length - 1]).toEqual({ t: 30, active: 0 });
	});

	test('single-shot drops after the last start', () => {
		expect(rampCurve(2, 10, 0)).toEqual([
			{ t: 0, active: 1 },
			{ t: 10, active: 2 },
			{ t: 10, active: 0 },
		]);
	});

	test('is empty for zero VUs', () => {
		expect(rampCurve(0, 10, 30)).toEqual([]);
	});
});

describe('observedConcurrency', () => {
	const req = (worker_id: number | null, sent_at: number): Request => ({
		id: 1,
		seq: 0,
		sent_at,
		duration_ms: null,
		method: 'GET',
		url: '/',
		status_code: 200,
		error: null,
		bytes_in: null,
		bytes_out: null,
		worker_id,
		headers: [],
		bodies: [],
	});

	test('counts distinct workers per second bucket', () => {
		const requests = [req(0, 1), req(1, 1), req(0, 2), req(2, 3)];
		expect(observedConcurrency(requests)).toEqual([
			{ t: 1, active: 2 },
			{ t: 2, active: 1 },
			{ t: 3, active: 1 },
		]);
	});

	test('ignores requests with no worker', () => {
		expect(observedConcurrency([req(null, 1), req(0, 1)])).toEqual([{ t: 1, active: 1 }]);
	});

	test('is empty for no requests', () => {
		expect(observedConcurrency([])).toEqual([]);
	});
});
