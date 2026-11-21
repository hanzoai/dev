// The Dev core as a library: `@hanzo/dev` (packages/dev), for a host that draws Dev
// inside its own shell. One ES entry; React stays the host's.
import react from "@vitejs/plugin-react";
import { tanstackRouter } from "@tanstack/router-plugin/vite";
import { defineConfig, type Plugin } from "vite-plus";

import { tailwindPlugins } from "./vite/tailwind";
import pkg from "./package.json" with { type: "json" };

// The Office engines are 75 to 108 MB of worker each; a host's static export cannot carry them.
// The embed draws one notice in their place (src/embed/OfficeUnavailable.tsx); the desktop
// app and the web client build with the engines as before.
const OFFICE_VIEWERS = /[\\/]files[\\/]office[\\/](WordDocument|Spreadsheet|Presentation)\.tsx$/;
function officeViewersOut(): Plugin {
  const replacement = new URL("./src/embed/OfficeUnavailable.tsx", import.meta.url).pathname;
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
          this.error(`${name} is ${(size / 1048576).toFixed(1)} MB; an embedded file may not pass 32 MB.`);
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
    embedSizeGuard(),
    tanstackRouter({ autoCodeSplitting: false }),
    react({ compiler: true }),
    tailwindPlugins(false),
  ],
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
        replacement: new URL("./vite/syncExternalStore.ts", import.meta.url).pathname,
      },
    ],
  },
  build: {
    outDir: "../../packages/dev/dist",
    emptyOutDir: true,
    cssCodeSplit: false,
    sourcemap: false,
    // `keymap` is its own small entry: a host resolves window keys before it has loaded the core.
    lib: {
      entry: { index: "src/embed/index.ts", keymap: "src/embed/keymap.ts" },
      formats: ["es"],
      cssFileName: "index",
    },
    // React stays the host's, all of it: `react/compiler-runtime` and `react/jsx-runtime` too. A bundled
    // copy of any of them calls `require("react")`, which a host's ES-module bundler cannot answer.
    rollupOptions: { external: (id: string) => /^react(-dom)?(\/|$)/.test(id) },
  },
});
