import { fileURLToPath } from "node:url";
import { defineConfig } from "vite-plus";

export default defineConfig({
  resolve: {
    alias: {
      "~": fileURLToPath(new URL("./builder", import.meta.url)),
    },
  },
  test: {
    environment: "node",
    include: ["builder/**/*.test.ts", "builder/**/*.test.tsx"],
  },
});
