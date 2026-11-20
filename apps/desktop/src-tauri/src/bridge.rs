use crate::auth::{client_platform, current_desktop_bootstrap_token, now_unix_ms};
use crate::backend::BackendRuntimeConfig;
use serde_json::json;

pub fn build_initialization_script(config: &BackendRuntimeConfig, local_enabled: bool) -> String {
    let bootstrap_token = current_desktop_bootstrap_token(&config.bootstrap_secret, now_unix_ms());
    let stage_label = if config.is_dev { "Dev" } else { "Beta" };
    let display_name = format!("Hanzo Dev ({stage_label})");
    let host_label = sysinfo::System::host_name().unwrap_or_else(|| "local".to_string());
    let locale = std::env::var("LC_ALL")
        .or_else(|_| std::env::var("LANG"))
        .ok()
        .and_then(|raw| {
            let base = raw.split('.').next()?.trim().replace('_', "-");
            (!base.is_empty() && base != "C" && base != "POSIX").then_some(base)
        })
        .unwrap_or_else(|| "en-US".to_string());

    let initial_state = json!({
        "clientPlatform": client_platform(),
        "branding": {
            "baseName": "Hanzo Dev",
            "stageLabel": stage_label,
            "displayName": display_name,
        },
        "systemLocale": locale,
        "preferredLanguages": [locale],
        "localEnvironmentEnabled": local_enabled,
        "bootstraps": [
            {
                "id": "primary",
                "label": host_label,
                "httpBaseUrl": config.http_base_url,
                "wsBaseUrl": config.ws_base_url,
                "bootstrapToken": bootstrap_token,
            }
        ],
    });

    let serialized_state =
        serde_json::to_string(&initial_state).unwrap_or_else(|_| "{}".to_string());

    format!(
        r#"(function () {{
  if (typeof window === "undefined" || window.desktopBridge) return;
  const __STATE__ = {serialized_state};
  let __fullscreen = false;
  let __localEnabled = __STATE__.localEnvironmentEnabled !== false;

  if (__STATE__.clientPlatform === "darwin") {{
    const applyInset = () => {{
      if (document.documentElement) {{
        document.documentElement.style.setProperty("--desktop-window-controls-inset", "90px");
      }}
    }};
    applyInset();
    window.addEventListener("DOMContentLoaded", applyInset, {{ once: true }});
  }}

  function invoke(cmd, args) {{
    if (window.__TAURI_INTERNALS__ && typeof window.__TAURI_INTERNALS__.invoke === "function") {{
      return window.__TAURI_INTERNALS__.invoke(cmd, args || {{}});
    }}
    if (window.__TAURI__ && window.__TAURI__.core && typeof window.__TAURI__.core.invoke === "function") {{
      return window.__TAURI__.core.invoke(cmd, args || {{}});
    }}
    return Promise.reject(new Error("Tauri IPC unavailable for " + cmd));
  }}

  function listenEvent(eventName, handler) {{
    let disposed = false;
    let unlisten = null;
    const attach = () => {{
      const eventApi = window.__TAURI__ && window.__TAURI__.event;
      if (!eventApi || typeof eventApi.listen !== "function") {{
        setTimeout(() => {{ if (!disposed) attach(); }}, 50);
        return;
      }}
      eventApi.listen(eventName, function (ev) {{
        if (!disposed) handler(ev.payload);
      }}).then(function (fn) {{
        if (disposed) fn();
        else unlisten = fn;
      }}).catch(function () {{}});
    }};
    attach();
    return function () {{
      disposed = true;
      if (typeof unlisten === "function") unlisten();
    }};
  }}

  listenEvent("desktop:window-fullscreen", function (value) {{
    if (typeof value === "boolean") __fullscreen = value;
  }});

  window.addEventListener("mousedown", function (event) {{
    if (event.button !== 0 || !(event.target instanceof Element)) return;
    if (!event.target.closest(".drag-region")) return;
    if (event.target.closest("button, a, input, textarea, select, [role='button'], [role='combobox'], [role='menuitem'], [data-sidebar='trigger'], [data-no-drag]")) return;
    if (event.detail === 2) {{
      invoke("desktop_toggle_maximize").catch(function () {{}});
    }} else if (event.detail === 1) {{
      invoke("desktop_start_dragging").catch(function () {{}});
    }}
  }});

  const noopUnsubscribe = function () {{ return function () {{}}; }};

  // Native Tauri in-app preview tab controller driving mounted preview iframes.
  let __nextPreviewId = 1;
  const __previewTabs = new Map();
  const __previewStateListeners = new Set();

  function __findPreviewIframe(tabId) {{
    if (typeof document === "undefined") return null;
    const nodes = document.querySelectorAll("iframe[data-tauri-preview-iframe]");
    for (let i = 0; i < nodes.length; i += 1) {{
      if (nodes[i].getAttribute("data-tauri-preview-iframe") === tabId) return nodes[i];
    }}
    return null;
  }}

  function __ensurePreviewTab(tabId, defaults) {{
    let tab = __previewTabs.get(tabId);
    if (!tab) {{
      tab = {{
        tabId: tabId,
        webContentsId: __nextPreviewId++,
        navStatus: {{ kind: "Idle" }},
        history: [],
        historyIndex: -1,
        zoomFactor: (defaults && typeof defaults.zoomFactor === "number" ? defaults.zoomFactor : 1),
        pictureInPicture: false,
        colorScheme: (defaults && defaults.colorScheme ? defaults.colorScheme : "system"),
        audioMuted: false,
        audible: false,
        controller: "human",
        capturing: false,
        updatedAt: new Date().toISOString(),
      }};
      __previewTabs.set(tabId, tab);
    }}
    return tab;
  }}

  function __emitPreviewState(tab) {{
    tab.updatedAt = new Date().toISOString();
    const snapshot = {{
      tabId: tab.tabId,
      webContentsId: tab.webContentsId,
      navStatus: tab.navStatus,
      canGoBack: tab.historyIndex > 0,
      canGoForward: tab.historyIndex >= 0 && tab.historyIndex < tab.history.length - 1,
      zoomFactor: tab.zoomFactor,
      pictureInPicture: tab.pictureInPicture,
      colorScheme: tab.colorScheme,
      audioMuted: tab.audioMuted,
      audible: tab.audible,
      controller: tab.controller,
      capturing: tab.capturing,
      updatedAt: tab.updatedAt,
    }};
    __previewStateListeners.forEach(function (listener) {{
      try {{ listener(tab.tabId, snapshot); }} catch (_err) {{}}
    }});
  }}

  function __navigatePreviewTab(tabId, url, pushHistory) {{
    const tab = __ensurePreviewTab(tabId);
    if (!url || url === "about:blank") {{
      tab.navStatus = {{ kind: "Idle" }};
      __emitPreviewState(tab);
      return;
    }}
    if (pushHistory) {{
      tab.history = tab.history.slice(0, tab.historyIndex + 1);
      if (tab.history[tab.history.length - 1] !== url) {{
        tab.history.push(url);
      }}
      tab.historyIndex = tab.history.length - 1;
    }}
    let title = url;
    try {{ title = new URL(url).host || url; }} catch (_e) {{}}
    tab.navStatus = {{ kind: "Loading", url: url, title: title }};
    __emitPreviewState(tab);
    const frame = __findPreviewIframe(tabId);
    if (frame && frame.src !== url) {{
      frame.src = url;
    }}
    setTimeout(function () {{
      if (__previewTabs.get(tabId) === tab && tab.navStatus.kind === "Loading" && tab.navStatus.url === url) {{
        tab.navStatus = {{ kind: "Success", url: url, title: title }};
        __emitPreviewState(tab);
      }}
    }}, 180);
  }}

  window.desktopBridge = {{
    getIamVault: () => invoke("desktop_get_iam_vault"),
    setIamVault: (vaultJson) => invoke("desktop_set_iam_vault", {{ vaultJson }}),
    discoverLanEngines: () => invoke("desktop_discover_lan_engines"),
    getAppBranding: () => __STATE__.branding,
    getRelayTelemetryEnabled: () => false,
    getPathForFile: (file) => (file && typeof file.path === "string" ? file.path : (file ? file.name : "")),
    getClientPlatform: () => __STATE__.clientPlatform,
    restartAudioService: async () => true,
    setNotificationBadge: async () => undefined,
    onNotificationBadgeClear: noopUnsubscribe,
    onTrackpadScrollBegin: noopUnsubscribe,
    onTrackpadScrollEnd: noopUnsubscribe,
    revealWindow: () => invoke("desktop_reveal_window"),
    getPrimaryPasteEnabled: () => true,
    getSystemLocale: () => __STATE__.systemLocale,
    getPreferredSystemLanguages: () => __STATE__.preferredLanguages,
    getLocalEnvironmentBootstraps: () => (__localEnabled ? __STATE__.bootstraps : []),
    getLocalEnvironmentBearerToken: () => invoke("desktop_get_local_environment_bearer_token"),
    getLocalEnvironmentEnabled: () => __localEnabled,
    setLocalEnvironmentEnabled: async (enabled) => {{
      __localEnabled = Boolean(enabled);
      await invoke("desktop_set_local_environment_enabled", {{ enabled: __localEnabled }});
    }},
    getHostEnvironmentId: () => invoke("desktop_get_host_environment_id"),
    getClientSettings: () => invoke("desktop_get_client_settings"),
    setClientSettings: (settings) => invoke("desktop_set_client_settings", {{ settings }}),
    getConnectionCatalog: () => invoke("desktop_get_connection_catalog"),
    setConnectionCatalog: (catalog) => invoke("desktop_set_connection_catalog", {{ catalog }}),
    discoverSshHosts: () => invoke("desktop_discover_ssh_hosts"),
    resolveSshHost: (alias) => invoke("desktop_resolve_ssh_host", {{ alias }}),
    requestSnapShotPermissions: (includeAccessibility) =>
      invoke("desktop_request_snapshot_permissions", {{ includeAccessibility: Boolean(includeAccessibility) }}),
    getSnapShotState: () => invoke("desktop_get_snapshot_state"),
    setupSnapShot: (action) => invoke("desktop_setup_snapshot", {{ action }}),
    previewSnapShotConfig: async () => null,
    applySnapShotConfig: async () => ({{ backupPath: null, warning: null }}),
    checkSnapShotShortcut: (shortcut) => invoke("desktop_check_snapshot_shortcut", {{ shortcut }}),
    setSnapShotShortcutSuppressed: (suppressed) =>
      invoke("desktop_set_snapshot_shortcut_suppressed", {{ suppressed: Boolean(suppressed) }}),
    listPendingSnapShots: () => invoke("desktop_list_pending_snapshots"),
    readSnapShot: (id) => invoke("desktop_read_snapshot", {{ id }}),
    setSnapShotAnimationDestination: async () => undefined,
    dismissSnapShotAnimation: async () => undefined,
    acknowledgeSnapShot: (id) => invoke("desktop_acknowledge_snapshot", {{ id }}),
    ensureSshEnvironment: (target) => invoke("desktop_ensure_ssh_environment", {{ target }}),
    disconnectSshEnvironment: (target) => invoke("desktop_disconnect_ssh_environment", {{ target }}),
    fetchSshEnvironmentDescriptor: async (httpBaseUrl) => {{
      const resp = await fetch(new URL("/.well-known/hanzo/environment", httpBaseUrl).toString());
      return await resp.json();
    }},
    bootstrapSshBearerSession: async (httpBaseUrl, credential) => {{
      const body = new URLSearchParams({{
        grant_type: "urn:ietf:params:oauth:grant-type:token-exchange",
        subject_token: credential,
        subject_token_type: "urn:hanzo:params:oauth:token-type:environment-bootstrap",
        requested_token_type: "urn:ietf:params:oauth:token-type:access_token",
      }});
      const resp = await fetch(new URL("/oauth/token", httpBaseUrl).toString(), {{
        method: "POST",
        headers: {{ "Content-Type": "application/x-www-form-urlencoded" }},
        body,
      }});
      return await resp.json();
    }},
    fetchSshSessionState: async (httpBaseUrl, bearerToken) => {{
      const resp = await fetch(new URL("/v1/auth/session", httpBaseUrl).toString(), {{
        headers: {{ Authorization: "Bearer " + bearerToken }},
      }});
      return await resp.json();
    }},
    onSshPasswordPrompt: noopUnsubscribe,
    onSshPasswordPromptSettled: noopUnsubscribe,
    resolveSshPasswordPrompt: async () => undefined,
    getServerExposureState: () => invoke("desktop_get_server_exposure_state"),
    setServerExposureMode: (mode) => invoke("desktop_set_server_exposure_mode", {{ mode }}),
    setTailscaleServeEnabled: () => invoke("desktop_get_server_exposure_state"),
    getAdvertisedEndpoints: async () => [],
    getWslState: async () => ({{
      enabled: false,
      distro: null,
      available: false,
      wslOnly: false,
      distros: [],
      preflightError: null,
    }}),
    setWslBackendEnabled: async () => ({{
      enabled: false,
      distro: null,
      available: false,
      wslOnly: false,
      distros: [],
      preflightError: null,
    }}),
    setWslDistro: async () => ({{
      enabled: false,
      distro: null,
      available: false,
      wslOnly: false,
      distros: [],
      preflightError: null,
    }}),
    setWslOnly: async () => ({{
      enabled: false,
      distro: null,
      available: false,
      wslOnly: false,
      distros: [],
      preflightError: null,
    }}),
    pickFolder: (options) => invoke("desktop_pick_folder", {{ options: options || null }}),
    toLocalBackendPath: async (_backendId, path) => path,
    pickProjectFavicon: (initialPath) => invoke("desktop_pick_project_favicon", {{ initialPath: initialPath || null }}),
    pickThemeFiles: () => invoke("desktop_pick_theme_files"),
    setTheme: (theme) => invoke("desktop_set_theme", {{ theme }}),
    getDockIconMode: () => invoke("get_dock_icon_mode"),
    setDockIconMode: (mode) => invoke("set_dock_icon_mode", {{ mode }}),
    onDockIconModeChange: (listener) => listenEvent("desktop:dock-icon-mode-changed", (mode) => {{
      if (mode === "branded" || mode === "minimal") listener(mode);
    }}),
    showContextMenu: (items, position) => invoke("desktop_show_context_menu", {{ items, position: position || null }}),
    receiveProviderAuthCallback: async () => "",
    cancelProviderAuthCallback: async () => undefined,
    startFileDrag: async () => false,
    openExternal: (url) => invoke("desktop_open_external", {{ url }}),
    readMedia: (url) => invoke("desktop_read_media", {{ url }}),
    checkSystemPermission: async () => true,
    openSystemSettings: (pane) => invoke("desktop_open_system_settings", {{ pane }}),
    probeRemoteEditors: () => invoke("desktop_probe_remote_editors"),
    pasteAsText: async () => undefined,
    onMenuAction: (listener) => listenEvent("desktop:menu-action", (action) => {{
      if (typeof action === "string") listener(action);
    }}),
    onSnapShotEvent: (listener) => listenEvent("desktop:snapshot-event", (event) => {{
      if (event && typeof event === "object") listener(event);
    }}),
    onQuitShortcut: noopUnsubscribe,
    getWindowFullscreenState: () => __fullscreen,
    getWindowZoomFactor: () => 1,
    onWindowFullscreenStateChange: (listener) => listenEvent("desktop:window-fullscreen", (fullscreen) => {{
      if (typeof fullscreen === "boolean") {{
        __fullscreen = fullscreen;
        listener(fullscreen);
      }}
    }}),
    getUpdateState: () => invoke("desktop_get_update_state"),
    setUpdateChannel: (channel) => invoke("desktop_set_update_channel", {{ channel }}),
    checkForUpdate: () => invoke("desktop_check_for_update"),
    downloadUpdate: () => invoke("desktop_download_update"),
    installUpdate: () => invoke("desktop_install_update"),
    onUpdateState: (listener) => listenEvent("desktop:update-state", listener),
    cliCommand: {{
      getState: () => invoke("desktop_cli_get_state"),
      install: () => invoke("desktop_cli_install"),
      uninstall: () => invoke("desktop_cli_uninstall"),
      addToPath: () => invoke("desktop_cli_add_to_path"),
    }},
    webLinks: {{
      setReady: (ready) => invoke("desktop_web_links_set_ready", {{ ready: Boolean(ready) }}),
      onOpen: (listener) => listenEvent("desktop:web-link-open", (url) => {{
        if (typeof url === "string") {{
          if (window.hanzo && typeof window.hanzo.track === "function") {{
            window.hanzo.track("deep_link_received", {{ scheme: url.split(":")[0] || "hanzodev" }});
          }}
          listener(url);
        }}
      }}),
    }},
    appActivation: {{
      setReady: async () => undefined,
      complete: async () => undefined,
      onRequest: noopUnsubscribe,
    }},
    preview: {{
      setForwardedShortcuts: async () => undefined,
      createTab: async (tabId, defaults) => {{
        const tab = __ensurePreviewTab(tabId, defaults);
        __emitPreviewState(tab);
      }},
      closeTab: async (tabId) => {{
        __previewTabs.delete(tabId);
      }},
      registerWebview: async (tabId, webContentsId) => {{
        const tab = __ensurePreviewTab(tabId);
        if (typeof webContentsId === "number" && webContentsId > 0) {{
          tab.webContentsId = webContentsId;
        }}
        const frame = __findPreviewIframe(tabId);
        if (frame && frame.src && frame.src !== "about:blank" && tab.navStatus.kind === "Idle") {{
          __navigatePreviewTab(tabId, frame.src, true);
        }} else {{
          __emitPreviewState(tab);
        }}
      }},
      navigate: async (tabId, url) => {{
        __navigatePreviewTab(tabId, url, true);
      }},
      goBack: async (tabId) => {{
        const tab = __previewTabs.get(tabId);
        if (!tab || tab.historyIndex <= 0) return;
        tab.historyIndex -= 1;
        __navigatePreviewTab(tabId, tab.history[tab.historyIndex], false);
      }},
      goForward: async (tabId) => {{
        const tab = __previewTabs.get(tabId);
        if (!tab || tab.historyIndex >= tab.history.length - 1) return;
        tab.historyIndex += 1;
        __navigatePreviewTab(tabId, tab.history[tab.historyIndex], false);
      }},
      refresh: async (tabId) => {{
        const tab = __previewTabs.get(tabId);
        const frame = __findPreviewIframe(tabId);
        if (frame && frame.src && frame.src !== "about:blank") {{
          const url = frame.src;
          frame.src = url;
          if (tab) __navigatePreviewTab(tabId, url, false);
        }}
      }},
      stopLoading: async (tabId) => {{
        const tab = __previewTabs.get(tabId);
        if (tab && tab.navStatus.kind === "Loading") {{
          tab.navStatus = {{ kind: "Success", url: tab.navStatus.url, title: tab.navStatus.title }};
          __emitPreviewState(tab);
        }}
      }},
      zoomIn: async (tabId) => {{
        const tab = __ensurePreviewTab(tabId);
        tab.zoomFactor = Math.min(3, Math.round((tab.zoomFactor + 0.1) * 100) / 100);
        __emitPreviewState(tab);
      }},
      zoomOut: async (tabId) => {{
        const tab = __ensurePreviewTab(tabId);
        tab.zoomFactor = Math.max(0.25, Math.round((tab.zoomFactor - 0.1) * 100) / 100);
        __emitPreviewState(tab);
      }},
      resetZoom: async (tabId) => {{
        const tab = __ensurePreviewTab(tabId);
        tab.zoomFactor = 1;
        __emitPreviewState(tab);
      }},
      setZoomFactor: async (tabId, zoomFactor) => {{
        const tab = __ensurePreviewTab(tabId);
        if (typeof zoomFactor === "number" && zoomFactor > 0) {{
          tab.zoomFactor = zoomFactor;
          __emitPreviewState(tab);
        }}
      }},
      hardReload: async (tabId) => {{
        const frame = __findPreviewIframe(tabId);
        if (frame && frame.src && frame.src !== "about:blank") {{
          const current = frame.src;
          frame.src = "about:blank";
          setTimeout(() => {{ frame.src = current; }}, 20);
        }}
      }},
      setColorScheme: async (tabId, colorScheme) => {{
        const tab = __ensurePreviewTab(tabId);
        tab.colorScheme = colorScheme || "system";
        __emitPreviewState(tab);
      }},
      setAudioMuted: async (tabId, audioMuted) => {{
        const tab = __ensurePreviewTab(tabId);
        tab.audioMuted = Boolean(audioMuted);
        __emitPreviewState(tab);
      }},
      findInPage: async () => undefined,
      stopFindInPage: async () => undefined,
      setShown: async () => undefined,
      setCaptureDrawn: async () => undefined,
      openDevTools: () => invoke("desktop_preview_open_devtools"),
      listBrowserImportSources: async () => [],
      importBrowserCookies: async () => ({{ importedCount: 0, skippedCount: 0 }}),
      clearCookies: async () => undefined,
      clearCache: async () => undefined,
      getPreviewConfig: async (_environmentId, profileId) => ({{
        partition: "persist:hanzo-preview-" + (profileId || "default"),
        webPreferences: "contextIsolation=yes",
        preloadUrl: null,
      }}),
      onTunnelAccessRequest: noopUnsubscribe,
      resolveTunnelAccess: async () => undefined,
      setAnnotationTheme: async () => undefined,
      setAnnotationSendEnabled: async () => undefined,
      annotationVoice: {{ onEvent: noopUnsubscribe, update: async () => undefined }},
      pickElement: async () => null,
      cancelPickElement: async () => undefined,
      captureScreenshot: (tabId) => invoke("desktop_preview_capture_screenshot", {{ tabId }}),
      revealArtifact: (path) => invoke("desktop_open_external", {{ url: "file://" + path }}).then(() => undefined),
      copyArtifactToClipboard: async () => undefined,
      pictureInPicture: {{
        open: async (tabId) => {{
          const tab = __ensurePreviewTab(tabId);
          tab.pictureInPicture = true;
          __emitPreviewState(tab);
        }},
        close: async (tabId) => {{
          const tab = __ensurePreviewTab(tabId);
          tab.pictureInPicture = false;
          __emitPreviewState(tab);
        }},
      }},
      recording: {{
        onInput: noopUnsubscribe,
        startScreencast: async () => undefined,
        stopScreencast: async () => undefined,
        save: (tabId, mimeType, data) =>
          invoke("desktop_preview_save_recording", {{
            tabId,
            mimeType,
            data: Array.from(data || []),
          }}),
      }},
      onStateChange: (listener) => {{
        if (typeof listener !== "function") return function () {{}};
        __previewStateListeners.add(listener);
        return function () {{ __previewStateListeners.delete(listener); }};
      }},
      onPointerEvent: noopUnsubscribe,
      onOpenLink: noopUnsubscribe,
      onFindResult: noopUnsubscribe,
    }},
  }};
}})();"#,
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::PathBuf;

    #[test]
    fn initialization_script_wires_native_v1_endpoints_and_tauri_bridge() {
        let cfg = BackendRuntimeConfig {
            port: 13773,
            bind_host: "127.0.0.1".to_string(),
            http_base_url: "http://127.0.0.1:13773".to_string(),
            ws_base_url: "ws://127.0.0.1:13773".to_string(),
            hanzo_home: PathBuf::from("/tmp/hanzo-dev-test"),
            state_dir: PathBuf::from("/tmp/hanzo-dev-test/dev"),
            bootstrap_secret: "0123456789abcdef0123456789abcdef".to_string(),
            host_scope: "abcdef0123456789abcdef0123456789".to_string(),
            is_dev: true,
        };
        let script = build_initialization_script(&cfg, true);
        assert!(script.contains("window.desktopBridge"));
        assert!(script.contains("desktop_get_local_environment_bearer_token"));
        assert!(script.contains("desktop_get_snapshot_state"));
        assert!(script.contains("desktop_ensure_ssh_environment"));
        assert!(script.contains("desktop_check_for_update"));
        assert!(script.contains("desktop_cli_install"));
        assert!(script.contains("get_dock_icon_mode"));
        assert!(script.contains("set_dock_icon_mode"));
        assert!(script.contains("data-tauri-preview-iframe"));
        assert!(script.contains("/v1/auth/session"));
        assert!(script.contains("/oauth/token"));
        assert!(!script.contains("/api/"));
    }

    #[test]
    fn default_capability_allows_local_and_loopback_dev_ports() {
        let raw = include_str!("../capabilities/default.json");
        let parsed: serde_json::Value =
            serde_json::from_str(raw).expect("valid capabilities/default.json");
        assert_eq!(parsed.get("local").and_then(|v| v.as_bool()), Some(true));
        let urls: Vec<&str> = parsed
            .pointer("/remote/urls")
            .and_then(|v| v.as_array())
            .expect("remote.urls array")
            .iter()
            .filter_map(|v| v.as_str())
            .collect();
        assert!(urls.contains(&"http://127.0.0.1:*"));
        assert!(urls.contains(&"http://localhost:*"));
    }
}
