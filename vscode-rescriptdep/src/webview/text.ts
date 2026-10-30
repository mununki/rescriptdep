// User-facing copy (designer-review1 WP-UI-3/5/6, designer-review3 R1). Set only through textContent.

export function plural(n: number, one: string, many: string): string {
  return `${n.toLocaleString('en-US')} ${n === 1 ? one : many}`;
}

export const copy = {
  fullTitle: 'Dependency Graph',
  unusedTitle: 'Modules Without Dependents',
  fullMeta: (modules: number, deps: number) =>
    `${plural(modules, 'module', 'modules')} · ${plural(deps, 'dependency', 'dependencies')}`,
  focusMeta: (dependents: number, dependencies: number) =>
    `${plural(dependents, 'dependent', 'dependents')} · ${plural(dependencies, 'dependency', 'dependencies')}`,
  unusedMeta: (modules: number) => `${plural(modules, 'module', 'modules')}${modules ? ' · entry points or possibly dead code' : ''}`,
  usedBy: (n: number) => `USED BY · ${n}`,
  uses: (n: number) => `USES · ${n}`,
  cycleLabel: 'Cyclic dependency',

  focusNotFoundTitle: 'Module not found',
  focusNotFound: (m: string) =>
    `${m} wasn't found in this project's build output. Build the project, or pick another module.`,
  isolated: (m: string) => `${m} has no dependencies and no dependents.`,
  unusedEmptyTitle: 'All modules have dependents',
  unusedEmpty: 'Every module has at least one dependent.',
  fullEmptyTitle: 'No modules found',
  fullEmpty: "No modules were found in this project's build output. Build the project and try again.",

  renderErrorTitle: "Couldn't render the graph",
  tooLarge: 'This graph is too large or too deep to lay out.',
  unreadable: "The graph data couldn't be read.",
  unknownError: (detail: string) => `Something went wrong while drawing the graph${detail ? ` (${detail})` : ''}.`,

  layingOut: (n: number) => `Laying out ${plural(n, 'module', 'modules')}…`,
  retry: 'Retry',
  pickModule: 'Focus on Module…',

  findPlaceholder: (mac: boolean) => `Find module (${mac ? '⌘F' : 'Ctrl+F'})`,
  noResults: 'No results',
  matchCount: (n: number) => plural(n, 'match', 'matches'),
  matchOf: (i: number, n: number) => `${i} of ${n}`,

  svgLabelFull: (n: number) => `Dependency graph, ${plural(n, 'module', 'modules')}`,
  svgLabelFocus: (m: string) => `Dependencies of ${m}`,
  svgLabelUnused: (n: number) => `Modules without dependents, ${plural(n, 'module', 'modules')}`,
  nodeLabel: (m: string, secondary: string | undefined, dependents: number, dependencies: number) =>
    `${m}${secondary ? ` (${secondary})` : ''}, ${plural(dependents, 'dependent', 'dependents')}, ${plural(dependencies, 'dependency', 'dependencies')}`,

  announceFull: (n: number, e: number) => `Dependency graph. ${copy.fullMeta(n, e)}.`,
  announceFocus: (m: string, dependents: number, dependencies: number) =>
    `Focused on ${m}. ${plural(dependents, 'dependent', 'dependents')}, ${plural(dependencies, 'dependency', 'dependencies')}.`,
  announceUnused: (n: number) => `${plural(n, 'module', 'modules')} without dependents.`,
  announceMatchOf: (i: number, n: number) => `${i} of ${n} matches`,
};
