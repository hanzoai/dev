// The Dev core as a library: `@hanzo/dev` (packages/dev), for a host that draws Dev
// inside its own shell. One ES entry; React stays the host's.
import react from "@vitejs/plugin-react";
import { tanstackRouter } from "@tanstack/router-plugin/vite";
import { defineConfig } from "vite-plus";

import { tailwindPlugins } from "./vite/tailwind";
import pkg from "./package.json" with { type: "json" };

export default defineConfig({
  assetsInclude: ["**/*.wasm"],
  plugins: [tanstackRouter({ autoCodeSplitting: false }), react({ compiler: true }), tailwindPlugins(false)],
  define: {
    "import.meta.env.VITE_WS_URL": JSON.stringify(""),
    "import.meta.env.VITE_HTTP_URL": JSON.stringify(""),
    "import.meta.env.VITE_HANZO_DEV_RELAY_URL": JSON.stringify(""),
    "import.meta.env.VITE_HOSTED_APP_URL": JSON.stringify(""),
    "import.meta.env.VITE_HOSTED_APP_CHANNEL": JSON.stringify(""),
    "import.meta.env.VITE_RELEASE_CHANNEL": JSON.stringify(""),
    "import.meta.env.APP_VERSION": JSON.stringify(pkg.version),
  },
  resolve: { tsconfigPaths: true, dedupe: ["react", "react-dom"] },
  build: {
    outDir: "../../packages/dev/dist",
    emptyOutDir: true,
    cssCodeSplit: false,
    sourcemap: false,
    lib: { entry: "src/embed/index.ts", formats: ["es"], fileName: "index" },
    rollupOptions: { external: ["react", "react-dom", "react/jsx-runtime", "react-dom/client"] },
  },
});
