// Fixture files are bundled as text by esbuild (`--loader:.dot=text`).
declare module '*.dot' {
  const text: string;
  export default text;
}
