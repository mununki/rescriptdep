const esbuild = require("esbuild");
const fs = require("fs");
const path = require("path");

const production = process.argv.includes('--production');
const watch = process.argv.includes('--watch');

/**
 * @type {import('esbuild').Plugin}
 */
const esbuildProblemMatcherPlugin = {
	name: 'esbuild-problem-matcher',

	setup(build) {
		build.onStart(() => {
			console.log('[watch] build started');
		});
		build.onEnd((result) => {
			result.errors.forEach(({ text, location }) => {
				console.error(`✘ [ERROR] ${text}`);
				if (location) {
					console.error(`    ${location.file}:${location.line}:${location.column}:`);
				}
			});
			console.log('[watch] build finished');
		});
	},
};

/** Extension host bundle (Node, CommonJS). */
const extensionOptions = {
	entryPoints: [
		'src/extension.ts'
	],
	bundle: true,
	format: 'cjs',
	minify: production,
	sourcemap: !production,
	sourcesContent: false,
	platform: 'node',
	outfile: 'dist/extension.js',
	external: ['vscode'],
	logLevel: 'silent',
	plugins: [
		/* add to the end of plugins array */
		esbuildProblemMatcherPlugin,
	],
};

/**
 * Graph webview bundle (browser): `dist/webview.js` (IIFE, viz-js bundled in, no CDN) and `dist/webview.css`.
 * The target matches the Chromium of the oldest supported VS Code (1.96 ships Electron 32 / Chromium 128).
 */
const webviewOptions = {
	entryPoints: [
		{ in: 'src/webview/main.ts', out: 'webview' },
		{ in: 'src/webview/styles.css', out: 'webview' },
	],
	bundle: true,
	format: 'iife',
	platform: 'browser',
	target: ['chrome120'],
	minify: production,
	sourcemap: !production,
	sourcesContent: false,
	legalComments: 'eof',
	outdir: 'dist',
	logLevel: 'silent',
	plugins: [
		esbuildProblemMatcherPlugin,
	],
};

/** Source maps this script writes in development builds; a production build must not ship stale ones. */
const SOURCE_MAPS = ['extension.js.map', 'webview.js.map', 'webview.css.map'];

async function main() {
	if (production) {
		for (const name of SOURCE_MAPS) {
			fs.rmSync(path.join(__dirname, 'dist', name), { force: true });
		}
	}
	const contexts = await Promise.all([
		esbuild.context(extensionOptions),
		esbuild.context(webviewOptions),
	]);
	if (watch) {
		await Promise.all(contexts.map((ctx) => ctx.watch()));
	} else {
		try {
			const results = await Promise.all(contexts.map((ctx) => ctx.rebuild()));
			if (results.some((r) => r.errors.length > 0)) {
				process.exitCode = 1;
			}
		} finally {
			await Promise.all(contexts.map((ctx) => ctx.dispose()));
		}
	}
}

main().catch(e => {
	console.error(e);
	process.exit(1);
});
