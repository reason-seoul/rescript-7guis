import { test } from 'uvu';
import * as assert from 'uvu/assert';

import { CircleDrawer } from './circle.bs.js';

test('transferHead', () => {
  let { transferHead, arrayToList, listToArray } = CircleDrawer.Util;

  let a1 = arrayToList([3, 2, 1]);
  let b1 = arrayToList([4, 5, 6]);

  let [a2, b2] = transferHead(a1, b1);

  assert.equal(listToArray(a2), [2, 1]);
  assert.equal(listToArray(b2), [3, 4, 5, 6]);
});

test.run();
