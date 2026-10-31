import { describe, expect, test } from 'bun:test';
import { makeLoad } from '$lib/test-utils';
import { load } from './+page';

describe('run detail page load', () => {
  test('marks run detail as not implemented', () => {
    expect(load(makeLoad('', { id: '1' }))).toEqual({ implemented: false, id: '1' });
  });
});
