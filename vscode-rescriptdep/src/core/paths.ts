// Path helpers for project containment, module files and the card's second line (QA G8/G9).
// Must not import 'vscode'.
import * as path from 'path';

/** Source extensions a module can be opened from, in preference order. */
export const SOURCE_EXTENSIONS = ['.res', '.resi', '.re', '.rei', '.ml', '.mli'] as const;

export function toPosix(p: string): string {
  return p.replace(/\\/g, '/');
}

/** `child` is `parent` or below it. Uses `path.relative`, never a string prefix (QA-4). */
export function isInside(child: string, parent: string, pathApi: path.PlatformPath = path): boolean {
  const rel = pathApi.relative(parent, child);
  if (rel === '') {
    return true;
  }
  return rel !== '..' && !rel.startsWith('..' + pathApi.sep) && !pathApi.isAbsolute(rel);
}

export function isSourceFile(p: string): boolean {
  const ext = path.extname(p).toLowerCase();
  return (SOURCE_EXTENSIONS as readonly string[]).includes(ext);
}

/** ReScript module name of a source file: the base name with its first letter capitalized. */
export function moduleNameFromFile(file: string): string {
  const base = path.basename(file, path.extname(file));
  return base.charAt(0).toUpperCase() + base.slice(1);
}

/** The module comes from a package (G9: dashed border iff the path contains `/node_modules/`). */
export function isExternalPath(p: string | undefined): boolean {
  return p !== undefined && toPosix(p).includes('/node_modules/');
}

/**
 * Package name from a path under `node_modules`, taken after the **last** `node_modules/` so pnpm's
 * `node_modules/.pnpm/pkg@1.0.0/node_modules/pkg/src` gives `pkg`. Scoped packages keep the scope.
 */
export function packageName(p: string): string | undefined {
  const posix = toPosix(p);
  const marker = '/node_modules/';
  const at = posix.lastIndexOf(marker);
  if (at < 0) {
    return undefined;
  }
  const segments = posix.slice(at + marker.length).split('/').filter((s) => s.length > 0);
  if (segments.length === 0) {
    return undefined;
  }
  if (segments[0].startsWith('@')) {
    return segments.length > 1 ? `${segments[0]}/${segments[1]}` : undefined;
  }
  return segments[0];
}

/** The deepest folder that contains every folder in the list. */
export function commonFolder(folders: readonly string[], pathApi: path.PlatformPath = path): string | undefined {
  if (folders.length === 0) {
    return undefined;
  }
  const split = folders.map((f) => pathApi.normalize(f).split(pathApi.sep));
  let common = split[0];
  for (const segments of split.slice(1)) {
    let i = 0;
    while (i < common.length && i < segments.length && common[i] === segments[i]) {
      i++;
    }
    common = common.slice(0, i);
  }
  if (common.length === 0) {
    return undefined;
  }
  const joined = common.join(pathApi.sep);
  // A posix root splits into ['', ...]; keep it as '/'.
  return joined === '' ? pathApi.sep : joined;
}

export interface SecondLineContext {
  /** Common folder of all project-module folders. */
  prefix: string | undefined;
  /** The project's modules live in more than one folder; otherwise no second line is shown. */
  multiFolder: boolean;
}

export function secondLineContext(projectModuleFiles: readonly string[], pathApi: path.PlatformPath = path): SecondLineContext {
  const folders = [...new Set(projectModuleFiles.map((f) => pathApi.dirname(pathApi.normalize(f))))];
  return { prefix: commonFolder(folders, pathApi), multiFolder: folders.length > 1 };
}

/**
 * The card's second line (designer-review3 §R1 "Card", QA G8):
 * - a module under `node_modules`: its package name;
 * - a project module: its folder relative to the common prefix of the project's module folders,
 *   or nothing when the project has a single folder (or the module sits in the prefix itself).
 */
export function secondLine(file: string | undefined, ctx: SecondLineContext, pathApi: path.PlatformPath = path): string | undefined {
  if (file === undefined) {
    return undefined;
  }
  if (isExternalPath(file)) {
    return packageName(file);
  }
  if (!ctx.multiFolder || ctx.prefix === undefined) {
    return undefined;
  }
  const folder = isSourceFile(file) ? pathApi.dirname(file) : file;
  if (!isInside(folder, ctx.prefix, pathApi)) {
    return undefined;
  }
  const rel = pathApi.relative(ctx.prefix, folder);
  return rel === '' ? undefined : toPosix(rel);
}

/**
 * Files whose base name matches `module` case-insensitively (QA-2: `MathUtil` → `mathUtil.res`),
 * ordered by extension preference (`.res` before `.resi`, …), then by path.
 */
export function matchModuleFiles(module: string, files: readonly string[]): string[] {
  const wanted = module.toLowerCase();
  const rank = (f: string) => (SOURCE_EXTENSIONS as readonly string[]).indexOf(path.extname(f).toLowerCase());
  return files
    .filter((f) => isSourceFile(f) && path.basename(f, path.extname(f)).toLowerCase() === wanted)
    .sort((a, b) => rank(a) - rank(b) || (a < b ? -1 : a > b ? 1 : 0));
}
