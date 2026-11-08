import { describe, expect, test } from 'bun:test';
import { previewRun } from './preview';

describe('previewRun', () => {
	test('single-shot: in-flight ceiling and total requests are foreseeable', () => {
		const p = previewRun({ name: 'x', script: '/s.hhs', nvu: 5, duration: 0, rampUp: 0 });
		expect(p.inFlightCeiling).toBe(5);
		expect(p.wallClock).toBe('single-shot');
		expect(p.totalRequests).toEqual({ known: true, value: 5 });
		expect(p.rateLimit.known).toBe(false);
	});

	test('timed run: total requests depends on the unresolved rate limit', () => {
		const p = previewRun({ name: 'x', script: '/s.hhs', nvu: 10, duration: 30, rampUp: 0 });
		expect(p.inFlightCeiling).toBe(10);
		expect(p.wallClock).toBe('30 seconds');
		expect(p.totalRequests.known).toBe(false);
		expect(p.rateLimit.known).toBe(false);
	});

	test('includes the planned ramp curve', () => {
		const p = previewRun({ name: 'x', script: '/s.hhs', nvu: 3, duration: 30, rampUp: 10 });
		expect(p.rampCurve[0]).toEqual({ t: 0, active: 1 });
		expect(p.rampCurve[1]).toEqual({ t: 5, active: 2 });
		expect(p.rampCurve[2]).toEqual({ t: 10, active: 3 });
		expect(p.rampCurve[p.rampCurve.length - 1]).toEqual({ t: 30, active: 0 });
	});

	test('clamps non-positive nvu to zero', () => {
		expect(
			previewRun({ name: 'x', script: '/s.hhs', nvu: -3, duration: 0, rampUp: 0 }).inFlightCeiling
		).toBe(0);
	});

	test('treats non-numeric nvu as zero', () => {
		expect(
			previewRun({ name: 'x', script: '/s.hhs', nvu: NaN, duration: 0, rampUp: 0 }).inFlightCeiling
		).toBe(0);
	});
});
