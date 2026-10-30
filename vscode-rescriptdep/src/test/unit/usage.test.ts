import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import { formatUsage, parseUsageJson, unavailableReason, usageCacheKey } from '../../core/usage';

const json = (counts: number[]) => JSON.stringify({ modules: counts.map((count, i) => ({ name: `M${i}`, count })) });
const text = (counts: number[]) => {
  const result = parseUsageJson(json(counts));
  assert.ok(result);
  return formatUsage(result);
};

test('sums only counts >= 0 (ARCH-5)', () => {
  assert.equal(parseUsageJson(json([2, 3]))?.total, 5);
  assert.equal(parseUsageJson(json([3, -3]))?.total, 3);
  assert.equal(parseUsageJson(json([-6, -6]))?.total, 0);
});

test('copy: 0 / 1 / N / N+ (C9)', () => {
  assert.equal(text([]), 'Used 0 times');
  assert.equal(text([0, 0]), 'Used 0 times');
  assert.equal(text([1]), 'Used 1 time');
  assert.equal(text([1, 0]), 'Used 1 time');
  assert.equal(text([2, 1]), 'Used 3 times');
  assert.equal(text([3, -3]), 'Used 3+ times');
  assert.equal(text([1, -4]), 'Used 1+ times');
});

test('all negative → nothing shown, never a negative number', () => {
  const result = parseUsageJson(json([-6, -6]));
  assert.deepEqual(result, { total: 0, partial: false, unavailable: true, codes: [-6, -6] });
  assert.equal(formatUsage(result!), undefined);
  for (const counts of [[-2], [-3, -4], [-5, 2], [-12]]) {
    assert.ok(!/-\d/.test(text(counts) ?? ''), `negative number shown for ${counts}`);
  }
});

test('unparseable output → undefined (never "?" or "error")', () => {
  assert.equal(parseUsageJson('garbage'), undefined);
  assert.equal(parseUsageJson('{"modules": 3}'), undefined);
  assert.equal(parseUsageJson('null'), undefined);
  assert.deepEqual(parseUsageJson('{"modules":[{"name":"A"},{"name":"B","count":"2"},{"name":"C","count":1}]}'), {
    total: 1,
    partial: false,
    unavailable: false,
    codes: [],
  });
});

test('the real CLI shape parses', () => {
  const out = '{\n  "modules": [\n    { "name": "Math", "count": 1 },\n    { "name": "Utils", "count": 0 }\n  ]\n}\n';
  assert.equal(formatUsage(parseUsageJson(out)!), 'Used 1 time');
});

test('status-bar reason follows the README code table', () => {
  assert.match(unavailableReason([-6, -6]), /this ReScript version/);
  assert.match(unavailableReason([-4]), /build output not found/);
  assert.match(unavailableReason([-3]), /\.cmt/);
  assert.match(unavailableReason([-2]), /implementation/);
  assert.match(unavailableReason([-5]), /file path/);
  assert.match(unavailableReason([-4, -6, -6]), /this ReScript version/);
  assert.match(unavailableReason([-99]), /aren't available/);
  for (const code of [-2, -3, -4, -5, -6]) {
    assert.match(unavailableReason([code]), /^Bibimbob: /);
  }
});

test('cache key: (file, line, name, .compiler.log mtime), with the old key as fallback', () => {
  const fallback = { projectRoot: '/p', sourceMtime: 1, bsMtime: 2, documentVersion: 3 };
  const a = usageCacheKey({ file: '/p/a.res', line: 1, name: 'x', compilerLogMtime: 10, fallback });
  assert.equal(a, usageCacheKey({ file: '/p/a.res', line: 1, name: 'x', compilerLogMtime: 10, fallback: { ...fallback, documentVersion: 9 } }));
  assert.notEqual(a, usageCacheKey({ file: '/p/a.res', line: 1, name: 'x', compilerLogMtime: 11, fallback }));
  assert.notEqual(a, usageCacheKey({ file: '/p/a.res', line: 2, name: 'x', compilerLogMtime: 10, fallback }));
  assert.notEqual(a, usageCacheKey({ file: '/p/a.res', line: 1, name: 'y', compilerLogMtime: 10, fallback }));
  const old = usageCacheKey({ file: '/p/a.res', line: 1, name: 'x', fallback });
  assert.notEqual(old, usageCacheKey({ file: '/p/a.res', line: 1, name: 'x', fallback: { ...fallback, documentVersion: 4 } }));
  assert.notEqual(old, usageCacheKey({ file: '/p/a.res', line: 1, name: 'x', fallback: { ...fallback, bsMtime: 5 } }));
});
