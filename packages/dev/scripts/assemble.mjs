import { copyFileSync, existsSync, writeFileSync } from "node:fs";
import { resolve } from "node:path";

const root = resolve(import.meta.dirname, "..");
const dist = resolve(root, "dist");

for (const required of ["core.js", "keymap.js", "index.css", "builder/index.js", "builder/index.d.ts"]) {
  const full = resolve(dist, required);
  if (!existsSync(full)) {
    console.error(`@hanzo/dev assemble: missing required build output ${required}`);
    process.exit(1);
  }
}

copyFileSync(resolve(root, "core.d.ts"), resolve(dist, "core.d.ts"));
copyFileSync(resolve(root, "keymap.d.ts"), resolve(dist, "keymap.d.ts"));

writeFileSync(
  resolve(dist, "index.js"),
  'export * from "./core.js";\nexport * from "./builder/index.js";\n',
  "utf8",
);

writeFileSync(
  resolve(dist, "index.d.ts"),
  'export * from "./core.js";\nexport * from "./builder/index.js";\n',
  "utf8",
);

console.log("@hanzo/dev dist assembled: index.js, index.d.ts, core.d.ts, keymap.d.ts");
