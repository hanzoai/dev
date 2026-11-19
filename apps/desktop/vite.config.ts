import "vite-plus/test/config";
import { defineConfig } from "vite-plus";

export default defineConfig({
  run: {
    tasks: {
      build: {
        command:
          "node scripts/stage-tauri-resources.mjs && cargo build --release --features custom-protocol --manifest-path src-tauri/Cargo.toml",
        dependsOn: ["@hanzodev/web#build", "hanzo#build"],
        cache: false,
      },
      "build:bundle": {
        command: "npx tauri build",
        dependsOn: ["@hanzodev/web#build", "hanzo#build"],
        cache: false,
      },
      dev: {
        command: "node scripts/dev-tauri.mjs",
        dependsOn: ["hanzo#build"],
        cache: false,
      },
      typecheck: {
        command: "cargo check --manifest-path src-tauri/Cargo.toml",
        cache: false,
      },
    },
  },
});
