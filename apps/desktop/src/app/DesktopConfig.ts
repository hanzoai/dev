import { OtlpHeadersFromString, OtlpProtocol } from "@hanzodev/shared/observability";
import * as Config from "effect/Config";
import * as ConfigProvider from "effect/ConfigProvider";
import * as Option from "effect/Option";

const trimNonEmptyOption = (value: string): Option.Option<string> => {
  const trimmed = value.trim();
  return trimmed.length > 0 ? Option.some(trimmed) : Option.none();
};

export const trimmedString = (name: string) =>
  Config.String(name).pipe(Config.option, Config.map(Option.flatMap(trimNonEmptyOption)));

const optionalBoolean = (name: string) =>
  Config.Boolean(name).pipe(Config.option, Config.map(Option.getOrElse(() => false)));

const commaSeparatedStrings = (name: string) =>
  trimmedString(name).pipe(
    Config.map(
      Option.match({
        onNone: () => [],
        onSome: (value) =>
          value
            .split(",")
            .map((entry) => entry.trim())
            .filter((entry) => entry.length > 0),
      }),
    ),
  );

const compactEnv = (env: Readonly<Record<string, string | undefined>>): Record<string, string> =>
  Object.fromEntries(
    Object.entries(env).filter((entry): entry is [string, string] => entry[1] !== undefined),
  );

export const DesktopConfig = Config.all({
  appDataDirectory: trimmedString("APPDATA"),
  xdgConfigHome: trimmedString("XDG_CONFIG_HOME"),
  xdgDataHome: trimmedString("XDG_DATA_HOME"),
  hanzoHome: trimmedString("HANZO_DEV_HOME"),
  devServerUrl: Config.URL("VITE_DEV_SERVER_URL").pipe(Config.option),
  appUserModelIdOverride: trimmedString("HANZO_DEV_DESKTOP_APP_USER_MODEL_ID"),
  devRemoteHanzoServerEntryPath: trimmedString("HANZO_DEV_DEV_REMOTE_HANZO_SERVER_ENTRY_PATH"),
  configuredBackendPort: Config.Port("HANZO_DEV_PORT").pipe(Config.option),
  commitHashOverride: trimmedString("HANZO_DEV_COMMIT_HASH"),
  desktopLanHostOverride: trimmedString("HANZO_DEV_DESKTOP_LAN_HOST"),
  desktopHttpsEndpointUrls: commaSeparatedStrings("HANZO_DEV_DESKTOP_HTTPS_ENDPOINTS"),
  otlpTracesUrl: trimmedString("HANZO_DEV_OTLP_TRACES_URL"),
  otlpMetricsUrl: trimmedString("HANZO_DEV_OTLP_METRICS_URL"),
  otlpLogsUrl: trimmedString("HANZO_DEV_OTLP_LOGS_URL"),
  otlpExportIntervalMs: Config.Int("HANZO_DEV_OTLP_EXPORT_INTERVAL_MS").pipe(
    Config.withDefault(10_000),
  ),
  otlpHeaders: Config.schema(OtlpHeadersFromString, "HANZO_DEV_OTLP_HEADERS").pipe(Config.option),
  otlpProtocol: Config.schema(OtlpProtocol, "HANZO_DEV_OTLP_PROTOCOL").pipe(
    Config.withDefault("http/json"),
  ),
  desktopLauncherPath: trimmedString("HANZO_DEV_DESKTOP_LAUNCHER_PATH"),
  appImagePath: trimmedString("APPIMAGE"),
  disableAutoUpdate: optionalBoolean("HANZO_DEV_DISABLE_AUTO_UPDATE"),
  mockUpdates: optionalBoolean("HANZO_DEV_DESKTOP_MOCK_UPDATES"),
  mockUpdateServerPort: Config.Port("HANZO_DEV_DESKTOP_MOCK_UPDATE_SERVER_PORT").pipe(
    Config.withDefault(3000),
  ),
});

export const layerTest = (env: Readonly<Record<string, string | undefined>>) =>
  ConfigProvider.layer(ConfigProvider.fromEnv({ env: compactEnv(env) }));
