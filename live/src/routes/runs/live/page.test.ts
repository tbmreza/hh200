import { describe, expect, test } from 'bun:test';
import { load } from './+page';

describe('live page load', () => {
  test('marks the live view as not implemented', () => {
    expect(load()).toEqual({ implemented: false });
  });
});
