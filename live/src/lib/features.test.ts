import { describe, expect, test } from 'bun:test';
import { features } from './features';

describe('features registry', () => {
  test('implemented endpoints are enabled', () => {
    expect(features.listRuns).toBe(true);
    expect(features.report).toBe(true);
    expect(features.signal).toBe(true);
    expect(features.sse).toBe(true);
  });

  test('unimplemented endpoints are disabled', () => {
    expect(features.runDetail).toBe(false);
    expect(features.liveRun).toBe(false);
    expect(features.startRun).toBe(false);
  });
});
