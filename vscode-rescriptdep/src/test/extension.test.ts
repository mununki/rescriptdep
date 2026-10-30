// Integration smoke test, run in a real VS Code by `pnpm test` (`vscode-test`). Not part of CI;
// the unit tests under `unit/` run with `pnpm run test:unit`.
import * as assert from 'assert';
import * as vscode from 'vscode';

const COMMANDS = [
	'bibimbob.showDependencyGraph',
	'bibimbob.focusModuleDependencies',
	'bibimbob.showUnusedModules',
	'bibimbob.toggleValueUsageCount',
	'bibimbob.graph.openModule',
	'bibimbob.graph.focusModule',
	'bibimbob.graph.copyModuleName',
	'bibimbob.graph.copyRelativePath',
	'bibimbob.graph.fitToView',
	'bibimbob.graph.back',
	'bibimbob.graph.showFullGraph',
];

suite('Extension Test Suite', () => {
	test('activates and registers every command', async () => {
		const extension = vscode.extensions.getExtension('mununki.vscode-bibimbob');
		assert.ok(extension, 'extension not found');
		await extension.activate();
		const registered = new Set(await vscode.commands.getCommands(true));
		for (const id of COMMANDS) {
			assert.ok(registered.has(id), `${id} is not registered`);
		}
	});

	test('graph commands without a panel are no-ops', async () => {
		await vscode.commands.executeCommand('bibimbob.graph.back');
		await vscode.commands.executeCommand('bibimbob.graph.focusModule', { panelId: 'nope', module: '../../etc/passwd' });
	});
});
