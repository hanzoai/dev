"use strict";

// Precede expo-widgets so its reverse-ordered mods create and wipe the target
// before native SwiftUI AskHanzo + HanzoBrief + Controls + AppIntents are copied and wired.

const path = require("path");
const fs = require("fs");
const { withDangerousMod, withXcodeProject } = require("expo/config-plugins");

const WIDGET_TARGET = "ExpoWidgetsTarget";
const APP_TARGET = "Hanzo";
const WIDGET_SWIFT_FILES = [
  "AskHanzo.swift",
  "HanzoBrief.swift",
  "HanzoAppIntents.swift",
  "HanzoControls.swift",
];
const APP_SWIFT_FILES = ["Hanzo/HanzoAppIntents.swift"];

function withSwiftSources(config) {
  return withDangerousMod(config, [
    "ios",
    (cfg) => {
      const srcDir = path.join(cfg.modRequest.projectRoot, "assets", "widget", "ios");
      const widgetDst = path.join(cfg.modRequest.platformProjectRoot, WIDGET_TARGET);
      const appDst = path.join(cfg.modRequest.platformProjectRoot, APP_TARGET);
      fs.mkdirSync(widgetDst, { recursive: true });
      fs.mkdirSync(appDst, { recursive: true });
      for (const file of WIDGET_SWIFT_FILES) {
        fs.copyFileSync(path.join(srcDir, file), path.join(widgetDst, file));
      }
      for (const file of APP_SWIFT_FILES) {
        const base = path.basename(file);
        fs.copyFileSync(path.join(srcDir, base), path.join(appDst, base));
      }
      const indexPath = path.join(widgetDst, "index.swift");
      if (fs.existsSync(indexPath)) {
        let indexSrc = fs.readFileSync(indexPath, "utf8");
        if (!indexSrc.includes("AskHanzo()")) {
          indexSrc = indexSrc.replace(
            "if #available(iOS 17.0, *) {",
            "if #available(iOS 17.0, *) {\n      AskHanzo()\n      HanzoBrief()",
          );
        }
        if (!indexSrc.includes("AskHanzoControl()")) {
          indexSrc = indexSrc.replace(
            "WidgetLiveActivity()",
            [
              "WidgetLiveActivity()",
              "    #if swift(>=6.0)",
              "    if #available(iOS 18.0, *) {",
              "      AskHanzoControl()",
              "      HanzoVoiceControl()",
              "      HanzoVisionControl()",
              "      HanzoDevControl()",
              "      HanzoLauncherControl()",
              "    }",
              "    #endif",
            ].join("\n"),
          );
        }
        fs.writeFileSync(indexPath, indexSrc);
      }
      return cfg;
    },
  ]);
}

function addFilesToTarget(project, targetName, files) {
  const target = project.pbxTargetByName(targetName);
  if (!target) return;
  const groupKey = project.findPBXGroupKey({ name: targetName });
  const targetEntry = Object.entries(project.pbxNativeTargetSection()).find(
    ([key, val]) => !key.endsWith("_comment") && val && val.name === targetName,
  );
  const targetUuid = targetEntry ? targetEntry[0] : null;
  for (const file of files) {
    project.addSourceFile(file, targetUuid ? { target: targetUuid } : undefined, groupKey);
  }
}

function withSwiftWiring(config) {
  return withXcodeProject(config, (cfg) => {
    addFilesToTarget(cfg.modResults, WIDGET_TARGET, WIDGET_SWIFT_FILES);
    addFilesToTarget(cfg.modResults, APP_TARGET, APP_SWIFT_FILES);
    return cfg;
  });
}

module.exports = function withAskHanzoWidgets(config) {
  return withSwiftWiring(withSwiftSources(config));
};
