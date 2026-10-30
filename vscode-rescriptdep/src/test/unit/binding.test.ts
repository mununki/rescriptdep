import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import { matchBinding } from '../../core/binding';

// QA-8 table (qa/evidence_ext.txt helpers) plus the cases the old regex got wrong.
const CASES: [string, string | undefined][] = [
  ['let rec loop = n => n', 'loop'],
  ['let add = (a, b) => a + b', 'add'],
  ['let \\"type" = 1', undefined],
  ['@genType let make = () => 1', 'make'],
  ['and other = 2', 'other'],
  ['let x: int = 1', 'x'],
  ['  let inner = 1', 'inner'],
  ['let make = (~a) => a == 1', 'make'],
  ['let (a, b) = (1, 2)', undefined],
  ['let _ = foo()', undefined],
  ['let promise = async () => 1', 'promise'],
  ["let x' = 1", "x'"],
  // more
  ['let rec fact = n => n <= 1 ? 1 : n * fact(n - 1)', 'fact'],
  ['and rec_helper = 2', 'rec_helper'],
  ['@react.component let make = () => React.null', 'make'],
  ['@module("fs") @genType.as("read") let readFile = 1', 'readFile'],
  ['let _unused = 1', '_unused'],
  ['let {a, b} = record', undefined],
  ['let Some(x) = y', undefined],
  ['letter = 1', undefined],
  ['type t = int', undefined],
  ['let rec', undefined],
  ['let add=(a, b) => a + b', 'add'],
  ['// let commented = 1', undefined],
  ['module M = {', undefined],
];

for (const [line, expected] of CASES) {
  test(`matchBinding(${JSON.stringify(line)}) = ${JSON.stringify(expected)}`, () => {
    assert.equal(matchBinding(line), expected);
  });
}
