import { faker } from '@faker-js/faker';

const OK_STATUSES = [200, 201, 204] as const;
const ERR_STATUSES = [400, 404, 429, 500, 502, 503] as const;

export function msAgo(rangeMs: number): bigint {
  return BigInt(Date.now()) - BigInt(faker.number.int({ min: 0, max: rangeMs }));
}

export function randomStatus(isFinished: boolean): string {
  if (!isFinished) return 'running';
  return faker.helpers.arrayElement(['completed', 'failed', 'cancelled']);
}

export function randomHttpStatus(isError: boolean): number {
  return isError
    ? faker.helpers.arrayElement([...ERR_STATUSES])
    : faker.helpers.arrayElement([...OK_STATUSES]);
}
