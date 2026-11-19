// Tauri initialization_script installs desktopBridge before web code runs, so module-load detection is reliable.
export const isTauri =
  typeof window !== "undefined" &&
  ("__TAURI_INTERNALS__" in window || "__TAURI__" in window);
export const isDesktop = typeof window !== "undefined" && window.desktopBridge !== undefined;
/** @deprecated Kept as alias for existing desktop shell checks; backed by Tauri v2 desktopBridge. */
export const isElectron = isDesktop;

export const isSecretsFeatureEnabled = ["1", "true"].includes(
  import.meta.env.HANZO_DEV_FEATURE_SECRETS?.trim().toLowerCase() ?? "",
);
