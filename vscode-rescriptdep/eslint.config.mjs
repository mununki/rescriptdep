import typescriptEslint from "@typescript-eslint/eslint-plugin";
import tsParser from "@typescript-eslint/parser";

// Globals of the graph webview (a browser bundle running inside a VS Code webview iframe).
const webviewGlobals = Object.fromEntries([
    "window", "document", "navigator", "console", "performance", "matchMedia", "getComputedStyle",
    "requestAnimationFrame", "cancelAnimationFrame", "setTimeout", "clearTimeout",
    "ResizeObserver", "MutationObserver", "DOMPoint", "MouseEvent", "KeyboardEvent", "PointerEvent",
    "WheelEvent", "FocusEvent", "Event", "Element", "HTMLElement", "HTMLInputElement", "HTMLButtonElement",
    "SVGElement", "SVGSVGElement", "SVGGElement", "SVGGraphicsElement", "MessageEvent", "acquireVsCodeApi",
].map((name) => [name, "readonly"]));

export default [{
    files: ["**/*.ts"],
}, {
    plugins: {
        "@typescript-eslint": typescriptEslint,
    },

    languageOptions: {
        parser: tsParser,
        ecmaVersion: 2022,
        sourceType: "module",
    },

    rules: {
        "@typescript-eslint/naming-convention": ["warn", {
            selector: "import",
            format: ["camelCase", "PascalCase"],
        }],

        curly: "warn",
        eqeqeq: "warn",
        "no-throw-literal": "warn",
        semi: "warn",
    },
}, {
    files: ["src/webview/**/*.ts"],

    languageOptions: {
        globals: webviewGlobals,
    },
}];
