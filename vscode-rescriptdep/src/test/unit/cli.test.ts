import { test } from 'node:test';
import * as assert from 'node:assert/strict';
import * as fs from 'fs';
import * as os from 'os';
import * as path from 'path';
import { RescriptDepCli, execCli, findOnPath, getBundledCLIPath } from '../../cli';
import { CliError, classifyCliError, cliErrorReason } from '../../core/cliError';
import { makeTree } from './helpers';
import { resetStub, stub } from './stub/vscode';

const unix = os.platform() !== 'win32';

test('classifyCliError: abort, maxBuffer, timeout, missing, failed (QA-6)', () => {
  assert.equal(classifyCliError({ name: 'AbortError', code: 'ABORT_ERR', killed: true, signal: 'SIGTERM' }), 'cancelled');
  assert.equal(classifyCliError({ code: 'ERR_CHILD_PROCESS_STDIO_MAXBUFFER', killed: true, signal: 'SIGTERM' }), 'tooLarge');
  // The real timeout error never mentions "timeout" in its message.
  assert.equal(classifyCliError({ message: 'Command failed: nice sleep 3\n', killed: true, signal: 'SIGTERM', code: null }), 'timeout');
  assert.equal(classifyCliError({ code: 'ENOENT' }), 'missing');
  assert.equal(classifyCliError({ code: 127 }), 'missing');
  assert.equal(classifyCliError({ code: 126 }), 'missing');
  assert.equal(classifyCliError({ code: 2 }), 'failed');
  assert.equal(classifyCliError({ killed: false, signal: 'SIGKILL' }), 'failed');
  assert.equal(classifyCliError(new CliError('timeout', 'x')), 'timeout');
  assert.equal(classifyCliError('boom'), 'failed');
});

test('error reasons', () => {
  assert.equal(cliErrorReason('timeout'), 'The CLI took longer than 10 seconds.');
  assert.equal(cliErrorReason('missing'), "The rescriptdep CLI couldn't be found.");
  assert.equal(cliErrorReason('failed', '\n  Fatal error: exception Not_found\nmore'), 'The CLI failed: Fatal error: exception Not_found');
  assert.equal(cliErrorReason('failed', ''), 'The CLI exited with an error.');
});

test('execCli: stdout through nice', { skip: !unix }, async () => {
  assert.equal(await execCli('/bin/echo', ['hello']), 'hello\n');
});

test('execCli: a timeout is reported as timeout', { skip: !unix }, async () => {
  const start = Date.now();
  await assert.rejects(execCli('sleep', ['5'], { timeoutMs: 200 }), (e: unknown) => e instanceof CliError && e.kind === 'timeout');
  assert.ok(Date.now() - start < 3000);
});

test('execCli: aborting kills the process and reports cancelled', { skip: !unix }, async () => {
  const abort = new AbortController();
  const start = Date.now();
  const run = execCli('sleep', ['5'], { signal: abort.signal });
  setTimeout(() => abort.abort(), 100);
  await assert.rejects(run, (e: unknown) => e instanceof CliError && e.kind === 'cancelled');
  assert.ok(Date.now() - start < 3000);
  // Already aborted: no spawn at all.
  await assert.rejects(execCli('sleep', ['5'], { signal: abort.signal }), (e: unknown) => e instanceof CliError && e.kind === 'cancelled');
});

test('execCli: a missing binary is reported as missing (nice exits 127)', { skip: !unix }, async () => {
  await assert.rejects(execCli('/nonexistent/rescriptdep', ['-f', 'dot']), (e: unknown) => e instanceof CliError && e.kind === 'missing');
});

test('execCli: a failing CLI keeps stderr for the reason line', { skip: !unix }, async () => {
  await assert.rejects(
    execCli('/bin/sh', ['-c', 'echo "Fatal error: boom" >&2; exit 3']),
    (e: unknown) => e instanceof CliError && e.kind === 'failed' && cliErrorReason(e.kind, e.stderr) === 'The CLI failed: Fatal error: boom',
  );
});

test('lookup order: dev build, then bundled binary (made executable), then PATH', { skip: !unix }, async () => {
  const root = makeTree({ 'ext/package.json': '{}', 'onpath/rescriptdep': '#!/bin/sh\necho path\n' });
  const ext = path.join(root, 'ext');
  fs.chmodSync(path.join(root, 'onpath/rescriptdep'), 0o755);

  assert.equal(await new RescriptDepCli(ext, () => true).resolvePath(), path.join(root, '_build/default/bin/main.exe'));

  const bundled = getBundledCLIPath(ext);
  fs.mkdirSync(path.dirname(bundled), { recursive: true });
  fs.writeFileSync(bundled, '#!/bin/sh\necho bundled\n', { mode: 0o644 });
  const cli = new RescriptDepCli(ext, () => false);
  assert.equal(await cli.resolvePath(), bundled);
  assert.ok((fs.statSync(bundled).mode & 0o111) !== 0, 'bundled binary made executable');
  assert.equal(await cli.run([]), 'bundled\n');

  fs.rmSync(bundled);
  assert.equal(await findOnPath('rescriptdep', path.join(root, 'onpath')), path.join(root, 'onpath/rescriptdep'));
  assert.equal(await findOnPath('rescriptdep', path.join(root, 'nothing-here')), undefined);
});

test('missing CLI: one message per session, memoized lookup, no spawn (QA-5)', { skip: !unix }, async () => {
  resetStub();
  const savedPath = process.env.PATH;
  process.env.PATH = makeTree({ 'empty/.keep': '' });
  try {
    const cli = new RescriptDepCli(makeTree({ 'package.json': '{}' }), () => false);
    for (let i = 0; i < 3; i++) {
      await assert.rejects(cli.run(['-f', 'dot']), (e: unknown) => e instanceof CliError && e.kind === 'missing');
    }
    assert.equal(stub.messages.length, 1);
    assert.match(stub.messages[0].text, /rescriptdep CLI cannot be found/);
    assert.deepEqual(stub.messages[0].items, ['Installation Guide']);
  } finally {
    process.env.PATH = savedPath;
  }
});

test('the install link points at the real repository (FE-15)', async () => {
  resetStub();
  stub.respond = (_text, items) => items[0];
  const cli = new RescriptDepCli('/nonexistent', () => false);
  cli.reportMissing();
  cli.reportMissing();
  await new Promise((r) => setTimeout(r, 10));
  if (unix) {
    assert.deepEqual(stub.opened, ['https://github.com/mununki/rescriptdep#installation']);
  }
  assert.equal(stub.messages.length, 1);
});
