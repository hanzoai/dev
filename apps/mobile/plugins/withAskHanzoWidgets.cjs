"use strict";

// Precede expo-widgets so its reverse-ordered mods create and wipe the target
// before native SwiftUI AskHanzo + HanzoBrief widgets are copied and wired.

const path = require("path");
const fs = require("fs");
const { withDangerousMod, withXcodeProject } = require("expo/config-plugins");

const TARGET_NAME = "ExpoWidgetsTarget";
const SWIFT_FILES = ["AskHanzo.swift", "HanzoBrief.swift"];

function withSwiftSources(config) {
  return withDangerousMod(config, [
    "ios",
    (cfg) => {
      const srcDir = path.join(cfg.modRequest.projectRoot, "assets", "widget", "ios");
      const dstDir = path.join(cfg.modRequest.platformProjectRoot, TARGET_NAME);
      fs.mkdirSync(dstDir, { recursive: true });
      for (const file of SWIFT_FILES) {
        fs.copyFileSync(path.join(srcDir, file), path.join(dstDir, file));
      }
      const indexPath = path.join(dstDir, "index.swift");
      if (fs.existsSync(indexPath)) {
        let indexSrc = fs.readFileSync(indexPath, "utf8");
        if (!indexSrc.includes("AskHanzo()")) {
          indexSrc = indexSrc.replace(
            "if #available(iOS 17.0, *) {",
            "if #available(iOS 17.0, *) {\n      AskHanzo()\n      HanzoBrief()",
          );
          fs.writeFileSync(indexPath, indexSrc);
        }
      }
      return cfg;
    },
  ]);
}

function withSwiftWiring(config) {
  return withXcodeProject(config, (cfg) => {
    const project = cfg.modResults;
    const target = project.pbxTargetByName(TARGET_NAME);
    if (!target) return cfg;
    const groupKey = project.findPBXGroupKey({ name: TARGET_NAME });
    const targetEntry = Object.entries(project.pbxNativeTargetSection()).find(
      ([key, val]) => !key.endsWith("_comment") && val && val.name === TARGET_NAME,
    );
    const targetUuid = targetEntry ? targetEntry[0] : null;
    for (const file of SWIFT_FILES) {
      if (project.hasFile(file)) continue;
      project.addSourceFile(
        file,
        targetUuid ? { target: targetUuid } : undefined,
        groupKey,
      );
    }
    return cfg;
  });
}

module.exports = function withAskHanzoWidgets(config) {
  return withSwiftWiring(withSwiftSources(config));
};
