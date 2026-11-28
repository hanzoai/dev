// The Dev core as a library: `@hanzo/dev` (packages/dev), for a host that draws Dev
// inside its own shell. One ES entry; React stays the host's.
import * as NodeURL from "node:url";

import react from "@vitejs/plugin-react";
import { tanstackRouter } from "@tanstack/router-plugin/vite";
import { defineConfig, type Plugin } from "vite-plus";

import { tailwindPlugins } from "./vite/tailwind";
import pkg from "./package.json" with { type: "json" };

// The packages the host bundles itself: React and the CommonJS that requires it.
const HOST_MODULES =
  /^(?:react(?:-dom)?|use-sync-external-store|@tanstack\/react-(?:router|store))(?:\/|$)/;

// The Office engines are 75 to 108 MB of worker each; a host's static export cannot carry them.
// The embed draws one notice in their place (src/embed/OfficeUnavailable.tsx); the desktop
// app and the web client build with the engines as before.
const OFFICE_VIEWERS = /[\\/]files[\\/]office[\\/](WordDocument|Spreadsheet|Presentation)\.tsx$/;
function officeViewersOut(): Plugin {
  const replacement = NodeURL.fileURLToPath(
    new URL("./src/embed/OfficeUnavailable.tsx", import.meta.url),
  );
  return {
    name: "hanzodev:embed-office-out",
    enforce: "pre",
    async resolveId(source, importer, options) {
      if (!source.startsWith("./office/")) return null;
      const resolved = await this.resolve(source, importer, { ...options, skipSelf: true });
      return resolved && OFFICE_VIEWERS.test(resolved.id) ? replacement : null;
    },
  };
}

// Embed-only slimming: strips standalone-desktop-only payloads (15 MB 3D device .glb meshes,
// 6.6 MB pdfjs-dist Electron fallback, 5.9 MB libheif WASM, 1.5 MB Nerd Font fallback) and
// unused @pierre/diffs Shiki grammars/themes (~35 MB / ~400 chunks — Dev uses `highlights` +
// `pierre-dark`/`pierre-light` everywhere). Standalone desktop/web builds stay untouched.
const DEVICE_MODELS = /[\\/]components[\\/]device[\\/]deviceModels\.ts$/;
const DEVICE_3D_VIEWPORTS =
  /[\\/]components[\\/]device[\\/](DevicePhoneViewport|DeviceDuoViewport)\.tsx$/;
const PDF_VIEWER = /[\\/]components[\\/]files[\\/]PdfViewer\.tsx$/;
const PIERRE_SHIKI_BACKEND =
  /[\\/]@pierre[\\/]diffs[\\/]dist[\\/](?:highlighter[\\/]backends[\\/]shiki\.js|worker[\\/]shiki-[^\\/]+\.js)$/;
const PIERRE_RESOLVE_LANGUAGE =
  /[\\/]@pierre[\\/]diffs[\\/]dist[\\/]highlighter[\\/]languages[\\/]resolveLanguage\.js$/;
const PIERRE_HIGHLIGHTS_THEME_LOADER =
  /[\\/]@pierre[\\/]highlights[\\/]dist[\\/]themes[\\/]loader\.js$/;
const PIERRE_THEMING_THEMES = /[\\/]@pierre[\\/]theming[\\/]dist[\\/]themes\.js$/;
const KATEX_CSS = /[\\/]katex[\\/]dist[\\/]katex(?:\.min)?\.css$/;
const TERMINAL_SYMBOLS_FONT = /SymbolsNerdFontMono-Regular\.woff2\?url$/;

const VIRTUAL_STUBS: Record<string, string> = {
  "\0hanzodev:embed-device-models": `export function deviceModel() { return null; }
export function deviceKeyboard() { return null; }
`,
  "\0hanzodev:embed-device-3d-stub": `import { useEffect } from "react";
function Unavailable3DViewport(props) {
  useEffect(() => { props.onUnavailable?.(); }, [props.onUnavailable]);
  return null;
}
export const DevicePhoneViewport = Unavailable3DViewport;
export const DeviceDuoViewport = Unavailable3DViewport;
`,
  "\0hanzodev:embed-pdf-fallback": `import { createElement } from "react";
export default function PdfFallback(props) {
  const base = String(props.src ?? "").split("#", 1)[0];
  return createElement("iframe", {
    src: base + "#navpanes=0&view=FitH",
    title: props.title,
    className: "block h-full min-h-0 w-full flex-1 border-0 bg-white",
  });
}
`,
  "\0hanzodev:embed-heic-stub": `export async function heicTo() {
  throw new Error("HEIC software decode is omitted in embedded builds.");
}
`,
  "\0hanzodev:embed-empty-url": `export default "";\n`,
  "\0hanzodev:embed-shiki-core-stub": `export function createCssVariablesTheme() {
  throw new Error("Shiki core is omitted in embedded builds.");
}
export function normalizeTheme() {
  throw new Error("Shiki core is omitted in embedded builds.");
}
export async function createHighlighterCore() {
  throw new Error("Shiki core is omitted in embedded builds.");
}
`,
};

const PIERRE_SHIKI_STUB = `export class ShikiHighlighter {
  static async create() {
    throw new Error("Shiki backend is omitted in embedded builds; use preferredHighlighter: 'highlights'.");
  }
}
`;

const PIERRE_RESOLVE_LANGUAGE_STUB = `export async function resolveLanguage(lang) {
  throw new Error(\`Shiki language "\${lang}" is omitted in embedded builds; use preferredHighlighter: 'highlights'.\`);
}
`;

const PIERRE_HIGHLIGHTS_THEMES_SLIM = `export const themes = {
  "pierre-dark": () => import("./pierre-dark.js"),
  "pierre-light": () => import("./pierre-light.js"),
};
`;

const PIERRE_THEMING_THEMES_SLIM = `export const pierreThemes = {};
export const shikiThemes = {};
export const themes = { getTheme: () => undefined };
export function createTheme() {
  throw new Error("Shiki themes are omitted in embedded builds.");
}
`;

function embedSlim(): Plugin {
  return {
    name: "hanzodev:embed-slim",
    enforce: "pre",
    async resolveId(source, importer, options) {
      if (source in VIRTUAL_STUBS) return source;
      if (source === "heic-to/csp") return "\0hanzodev:embed-heic-stub";
      if (source === "shiki/core") return "\0hanzodev:embed-shiki-core-stub";
      if (TERMINAL_SYMBOLS_FONT.test(source)) return "\0hanzodev:embed-empty-url";

      if (
        source.includes("deviceModels") ||
        source.includes("DevicePhoneViewport") ||
        source.includes("DeviceDuoViewport") ||
        source.includes("PdfViewer")
      ) {
        const resolved = await this.resolve(source, importer, { ...options, skipSelf: true });
        if (!resolved) return null;
        if (DEVICE_MODELS.test(resolved.id)) return "\0hanzodev:embed-device-models";
        if (DEVICE_3D_VIEWPORTS.test(resolved.id)) return "\0hanzodev:embed-device-3d-stub";
        if (PDF_VIEWER.test(resolved.id)) return "\0hanzodev:embed-pdf-fallback";
      }
      return null;
    },
    load(id) {
      if (id in VIRTUAL_STUBS) return VIRTUAL_STUBS[id]!;
      const cleanId = id.split("?", 1)[0]!;
      if (PIERRE_SHIKI_BACKEND.test(cleanId)) return PIERRE_SHIKI_STUB;
      if (PIERRE_RESOLVE_LANGUAGE.test(cleanId)) return PIERRE_RESOLVE_LANGUAGE_STUB;
      if (PIERRE_HIGHLIGHTS_THEME_LOADER.test(cleanId)) return PIERRE_HIGHLIGHTS_THEMES_SLIM;
      if (PIERRE_THEMING_THEMES.test(cleanId)) return PIERRE_THEMING_THEMES_SLIM;
      return null;
    },
    transform(code, id) {
      // Every supported browser/WebView reads woff2; drop legacy woff/ttf fallbacks (~1.1 MB).
      if (KATEX_CSS.test(id.split("?", 1)[0]!)) {
        return code.replace(
          /,url\(fonts\/[^)]+\.(?:woff|ttf)\) format\("(?:woff|truetype)"\)/g,
          "",
        );
      }
      return null;
    },
  };
}

// A host's static export is published file by file. 15 MB files were accepted and 75 MB ones
// were refused (hanzo-inc/app deploy), so no file this library ships may pass 32 MB.
const MAX_FILE_BYTES = 32 * 1024 * 1024;
function embedSizeGuard(): Plugin {
  return {
    name: "hanzodev:embed-size-guard",
    generateBundle(_options, bundle) {
      for (const [name, file] of Object.entries(bundle)) {
        const size = file.type === "chunk" ? Buffer.byteLength(file.code) : file.source.length;
        if (size > MAX_FILE_BYTES) {
          this.error(
            `${name} is ${(size / 1048576).toFixed(1)} MB; an embedded file may not pass 32 MB.`,
          );
        }
      }
    },
  };
}

export default defineConfig({
  // Relative, so a worker the library ships is addressed from the file that starts it; an
  // absolute "/assets/..." is the host's own server root, which a host's bundler cannot resolve.
  base: "./",
  publicDir: false, // the host serves its own public files; the library ships code and its own assets
  assetsInclude: ["**/*.wasm"],
  plugins: [
    officeViewersOut(),
    embedSlim(),
    embedSizeGuard(),
    tanstackRouter({ autoCodeSplitting: false }),
    react({ compiler: true }),
    tailwindPlugins(false),
  ],
  worker: {
    plugins: () => [embedSlim()],
  },
  define: {
    "import.meta.env.VITE_WS_URL": JSON.stringify(""),
    "import.meta.env.VITE_HTTP_URL": JSON.stringify(""),
    "import.meta.env.VITE_HANZO_DEV_RELAY_URL": JSON.stringify(""),
    "import.meta.env.VITE_HOSTED_APP_URL": JSON.stringify(""),
    "import.meta.env.VITE_HOSTED_APP_CHANNEL": JSON.stringify(""),
    "import.meta.env.VITE_RELEASE_CHANNEL": JSON.stringify(""),
    "import.meta.env.APP_VERSION": JSON.stringify(pkg.version),
  },
  resolve: {
    tsconfigPaths: true,
    dedupe: ["react", "react-dom"],
    alias: [
      {
        find: /^use-sync-external-store\/(shim(\/index\.js)?|(shim\/)?with-selector(\.js)?)$/,
        replacement: NodeURL.fileURLToPath(new URL("./vite/syncExternalStore.ts", import.meta.url)),
      },
      {
        find: /^decode-named-character-reference$/,
        replacement: NodeURL.fileURLToPath(
          new URL("./vite/decodeNamedCharacterReference.ts", import.meta.url),
        ),
      },
    ],
  },
  build: {
    outDir: "../../packages/dev/dist",
    emptyOutDir: true,
    cssCodeSplit: false,
    minify: true,
    cssMinify: true,
    sourcemap: false,
    // `core` is the embedded Dev core; `keymap` is its own small entry so a host resolves window
    // keys before loading the core. `packages/dev` assembles `dist/index.js` over `core` + `builder`.
    lib: {
      entry: { core: "src/embed/index.ts", keymap: "src/embed/keymap.ts" },
      formats: ["es"],
      cssFileName: "index",
    },
    // React stays the host's, all of it, and so does whatever reaches it through CommonJS
    // (`use-sync-external-store`, behind the router): a bundled copy calls `require("react")`,
    // which a host's ES-module bundler cannot answer. The host installs them (package.json).
    rolldownOptions: {
      external: (id: string) => HOST_MODULES.test(id),
      output: { minify: true },
    },
  },
});
