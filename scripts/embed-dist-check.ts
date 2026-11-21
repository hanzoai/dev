#!/usr/bin/env node
// @effect-diagnostics nodeBuiltinImport:off globalConsole:off - A one-shot release gate; synchronous Node APIs keep it small.
// The embedded Dev core is bundled by its host. A host's ES-module bundler cannot answer a CommonJS
// `require("react")`, so no file of the library may call one (the minified helper reads `n("react")`),
// and every package the library imports bare must be one the host installs: a dependency or a peer.
import * as NodeFS from "node:fs";
import * as NodePath from "node:path";

const root = NodePath.resolve(import.meta.dirname, "..", "packages", "dev");
const dist = NodePath.join(root, "dist");
const manifest = JSON.parse(NodeFS.readFileSync(NodePath.join(root, "package.json"), "utf8"));
const installed = new Set([
  ...Object.keys(manifest.dependencies ?? {}),
  ...Object.keys(manifest.peerDependencies ?? {}),
]);
// A statement that starts a line: `import "x"`, `import { a } from "x"`, `export * from "x"`, and `import("x")`.
const IMPORTED =
  /^(?:import|export)\s*(?:[^'";]*?\bfrom\s*|\s*)["']([A-Za-z@][^"']*)["']|\bimport\(\s*["']([A-Za-z@][^"']*)["']\s*\)/gm;
const packageOf = (specifier: string) =>
  specifier
    .split("/")
    .slice(0, specifier.startsWith("@") ? 2 : 1)
    .join("/");

const files = NodeFS.readdirSync(dist, { recursive: true, encoding: "utf8" }).filter((name) =>
  name.endsWith(".js"),
);
const problems: string[] = [];
for (const name of files) {
  const code = NodeFS.readFileSync(NodePath.join(dist, name), "utf8");
  if (/\(\s*["']react["']\s*\)/.test(code)) problems.push(`${name} calls require("react")`);
  for (const match of code.matchAll(IMPORTED)) {
    const specifier = (match[1] ?? match[2])!;
    if (!installed.has(packageOf(specifier))) {
      problems.push(
        `${name} imports ${specifier}, which package.json does not list as a dependency or a peer`,
      );
    }
  }
}
if (files.length === 0) problems.push("dist holds no files: the library was not built");
if (problems.length > 0) {
  console.error([...new Set(problems)].join("\n"));
  process.exit(1);
}
console.log(
  `embed dist: ${files.length} files, no require of react, every bare import is installed by the host`,
);
