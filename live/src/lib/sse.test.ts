import { describe, expect, test } from 'bun:test';
import type { Run } from '$lib/types';
import { parseSse } from './sse';

const run: Run = {
	id: 1,
	name: 'a',
	status: 'running',
	script_path: '/s',
	concurrency: 1,
	rate_limit: 1,
	started_at: 0,
	ended_at: null,
	control_socket: '/tmp/x',
};

describe('parseSse', () => {
	test('parses a NewRow event', () => {
		expect(parseSse(JSON.stringify({ kind: 'NewRow', run }))).toEqual({ kind: 'NewRow', run });
	});

	test('parses an UpdateRow event', () => {
		expect(parseSse(JSON.stringify({ kind: 'UpdateRow', id: 1, patch: { status: 'completed' } }))).toEqual({
			kind: 'UpdateRow',
			id: 1,
			patch: { status: 'completed' },
		});
	});

	test('parses a NoNews event', () => {
		expect(parseSse(JSON.stringify({ kind: 'NoNews' }))).toEqual({ kind: 'NoNews' });
	});

	test('treats a bare run object as a NewRow', () => {
		expect(parseSse(JSON.stringify(run))).toEqual({ kind: 'NewRow', run });
	});

	test('ignores unknown placeholder payloads', () => {
		expect(parseSse(JSON.stringify({ nested: { inner: 1 }, float: 1.5 }))).toBeNull();
	});

	test('ignores invalid JSON', () => {
		expect(parseSse('not json')).toBeNull();
	});
});
