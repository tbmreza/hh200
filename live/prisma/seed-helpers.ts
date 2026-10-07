import { faker } from '@faker-js/faker';
import type { RunStatus } from '../src/lib/types';

const OK_STATUSES = [200, 201, 204] as const;
const ERR_STATUSES = [400, 404, 429, 500, 502, 503] as const;
const FINISHED_STATUSES = ['completed', 'failed', 'cancelled'] as const;

// Unix epoch seconds, matching the backend's `unixepoch('now')`.
export function secondsAgo(rangeSeconds: number): bigint {
	return BigInt(Math.floor(Date.now() / 1000)) - BigInt(faker.number.int({ min: 0, max: rangeSeconds }));
}

export function randomStatus(isFinished: boolean): RunStatus {
	if (!isFinished) return 'running';
	return faker.helpers.arrayElement([...FINISHED_STATUSES]);
}

export function randomHttpStatus(isError: boolean): number {
	return isError
		? faker.helpers.arrayElement([...ERR_STATUSES])
		: faker.helpers.arrayElement([...OK_STATUSES]);
}
