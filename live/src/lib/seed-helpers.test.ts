import { describe, expect, test } from 'bun:test';
import { faker } from '@faker-js/faker';
import { msAgo, randomHttpStatus, randomStatus } from './seed-helpers';

describe('randomStatus', () => {
  test('unfinished runs are always running', () => {
    for (let i = 0; i < 100; i++) {
      expect(randomStatus(false)).toBe('running');
    }
  });

  test('finished runs return a terminal status', () => {
    faker.seed(42);
    const seen = new Set<string>();
    for (let i = 0; i < 1000; i++) {
      seen.add(randomStatus(true));
    }
    expect([...seen].sort()).toEqual(['cancelled', 'completed', 'failed']);
  });
});

describe('randomHttpStatus', () => {
  test('errors come from the error statuses', () => {
    const allowed = [400, 404, 429, 500, 502, 503];
    for (let i = 0; i < 1000; i++) {
      expect(allowed).toContain(randomHttpStatus(true));
    }
  });

  test('non-errors come from the ok statuses', () => {
    const allowed = [200, 201, 204];
    for (let i = 0; i < 1000; i++) {
      expect(allowed).toContain(randomHttpStatus(false));
    }
  });
});

describe('msAgo', () => {
  test('returns a bigint in the past within rangeMs', () => {
    faker.seed(1);
    const rangeMs = 3_600_000;
    for (let i = 0; i < 1000; i++) {
      const ago = msAgo(rangeMs);
      const diff = Date.now() - Number(ago);
      expect(typeof ago).toBe('bigint');
      expect(diff).toBeGreaterThanOrEqual(0);
      expect(diff).toBeLessThanOrEqual(rangeMs);
    }
  });
});