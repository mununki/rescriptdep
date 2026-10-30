import { defineConfig } from '@vscode/test-cli';

export default defineConfig({
	// Only the integration suite; `out/test/unit` holds the `node --test` unit tests (`pnpm run test:unit`).
	files: 'out/test/*.test.js',
});
