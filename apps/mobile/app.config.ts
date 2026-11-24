import type { ExpoConfig } from "expo/config";

import { loadRepoEnv } from "../../scripts/lib/public-config.ts";

type AppVariant = "development" | "preview" | "production";

const repoEnv = loadRepoEnv();
Object.assign(process.env, repoEnv);
// Expo's TypeScript setup otherwise installs `typescript` on every start; we typecheck with tsrs.
process.env.EXPO_NO_TYPESCRIPT_SETUP = "1";

const APP_VARIANT = resolveAppVariant(repoEnv.APP_VARIANT);
const isIosPersonalTeamBuild = repoEnv.HANZO_DEV_IOS_PERSONAL_TEAM === "1";
const easProjectId = repoEnv.HANZO_DEV_EAS_PROJECT_ID?.trim() || undefined;
const runtimeVersionPolicy =
  process.env.MOBILE_VERSION_POLICY ??
  (APP_VARIANT === "development" ? "appVersion" : "fingerprint");

const personalTeamBundleIdentifier = repoEnv.HANZO_DEV_IOS_PERSONAL_TEAM_BUNDLE_ID?.trim();
const explicitIosBundleIdentifier = repoEnv.HANZO_DEV_IOS_BUNDLE_ID?.trim();
const IOS_BUNDLE_IDENTIFIER_PATTERN = /^[A-Za-z0-9-]+(?:\.[A-Za-z0-9-]+)+$/;

// Compiled into the native project. Without a relay, Observe's native side could dispatch to
// Expo's own ingest before observe.ts turns it off; `.invalid` never resolves (RFC 6761).
const observeEndpointUrl = repoEnv.HANZO_DEV_RELAY_URL?.trim()
  ? new URL("/v1/mobile/observe", repoEnv.HANZO_DEV_RELAY_URL.trim()).toString()
  : "https://observe.invalid";

const fromRepoRoot = (relativePath: string) => `../../${relativePath}`;
// The default icon of every variant: the plain Hanzo H, as hanzo.app. `</>` with an H and `</>` are alternates.
const HANZO_ICON = fromRepoRoot("assets/app-icons/hanzo-1024.png");
const androidAdaptiveForeground = "./assets/android-icon-foreground.png";

if (
  isIosPersonalTeamBuild &&
  (!personalTeamBundleIdentifier ||
    !IOS_BUNDLE_IDENTIFIER_PATTERN.test(personalTeamBundleIdentifier))
) {
  throw new Error(
    "HANZO_DEV_IOS_PERSONAL_TEAM_BUNDLE_ID must be a reverse-DNS identifier such as com.example.hanzodev when HANZO_DEV_IOS_PERSONAL_TEAM=1.",
  );
}

const DEVELOPMENT_ASSETS = {
  appIcon: HANZO_ICON,
  iosIcon: HANZO_ICON,
  splashIcon: HANZO_ICON,
  androidAdaptiveForeground,
  androidAdaptiveBackgroundColor: "#347FF8",
  androidAdaptiveBackgroundImage: "./assets/android-icon-background-dev.png",
  androidSplashIcon: "./assets/android-splash-icon-dev.png",
  androidMonochromeIcon: "./assets/android-icon-mark.png",
  androidNotificationIcon: "./assets/android-notification-icon.png",
  androidNotificationColor: "#00639B",
} as const;

const PREVIEW_ASSETS = {
  appIcon: HANZO_ICON,
  iosIcon: HANZO_ICON,
  splashIcon: HANZO_ICON,
  androidAdaptiveForeground,
  androidAdaptiveBackgroundColor: "#111533",
  androidAdaptiveBackgroundImage: "./assets/android-icon-background-nightly.png",
  androidSplashIcon: "./assets/android-splash-icon-nightly.png",
  androidMonochromeIcon: "./assets/android-icon-mark.png",
  androidNotificationIcon: "./assets/android-notification-icon.png",
  androidNotificationColor: "#7565C7",
} as const;

const RELEASE_ASSETS = {
  appIcon: HANZO_ICON,
  iosIcon: HANZO_ICON,
  splashIcon: HANZO_ICON,
  androidAdaptiveForeground,
  androidAdaptiveBackgroundColor: "#000000",
  androidAdaptiveBackgroundImage: undefined,
  androidSplashIcon: "./assets/android-splash-icon-prod.png",
  androidMonochromeIcon: "./assets/android-icon-mark.png",
  androidNotificationIcon: "./assets/android-notification-icon.png",
  androidNotificationColor: "#FFFFFF",
} as const;

const VARIANT_CONFIG = {
  development: {
    appName: "Hanzo",
    scheme: "hanzo-dev",
    iosBundleIdentifier: "ai.hanzo.dev.dev",
    androidPackage: "ai.hanzo.dev.dev",
    relyingParty: "iam.hanzo.ai",
    assets: DEVELOPMENT_ASSETS,
  },
  preview: {
    appName: "Hanzo",
    scheme: "hanzo-preview",
    iosBundleIdentifier: "ai.hanzo.dev.preview",
    androidPackage: "ai.hanzo.dev.preview",
    relyingParty: "iam.hanzo.ai",
    assets: PREVIEW_ASSETS,
  },
  production: {
    appName: "Hanzo",
    scheme: "hanzodev",
    iosBundleIdentifier: "ai.hanzo.dev",
    androidPackage: "ai.hanzo.dev",
    relyingParty: "iam.hanzo.ai",
    assets: RELEASE_ASSETS,
  },
} as const;

function resolveAppVariant(value: string | undefined): AppVariant {
  switch (value) {
    case "development":
    case "preview":
    case "production":
      return value;
    case undefined:
      return "production";
    default:
      throw new Error("APP_VARIANT must be development, preview, or production.");
  }
}

const variant = VARIANT_CONFIG[APP_VARIANT];
// Copied thread links open the app. hanzo.bot/.well-known lists every variant's identifiers.
const HOSTED_APP_HOST = "hanzo.bot";
const iosBundleIdentifier = isIosPersonalTeamBuild
  ? personalTeamBundleIdentifier!
  : explicitIosBundleIdentifier && IOS_BUNDLE_IDENTIFIER_PATTERN.test(explicitIosBundleIdentifier)
    ? explicitIosBundleIdentifier
    : variant.iosBundleIdentifier;

const zenFonts = {
  regular: "@hanzo/font/dist/fonts/zen-sans/Zen-Regular.ttf",
  medium: "@hanzo/font/dist/fonts/zen-sans/Zen-Medium.ttf",
  bold: "@hanzo/font/dist/fonts/zen-sans/Zen-Bold.ttf",
} as const;

const widgetsPlugin: NonNullable<ExpoConfig["plugins"]>[number] = [
  "expo-widgets",
  {
    bundleIdentifier: `${iosBundleIdentifier}.widgets`,
    groupIdentifier: `group.${iosBundleIdentifier}`,
    enablePushNotifications: true,
    // Frequent updates need the iOS entitlement to avoid early throttling.
    frequentUpdates: true,
    enableAndroid: true,
    widgets: [
      {
        name: "SubscriptionUsage",
        displayName: "Subscription usage",
        description: "Subscription quotas from your connected Hanzo Dev environments.",
        ios: {
          configuration: {
            title: "Subscription usage",
            description:
              "Both shows Session and Weekly when available. The Lock Screen shows the tightest selected limit.",
            parameters: {
              codexPeriod: {
                title: "Codex limits",
                type: "enum",
                default: "auto",
                values: [
                  { name: "Both", value: "auto" },
                  { name: "Session", value: "session" },
                  { name: "Weekly", value: "weekly" },
                ],
              },
              claudePeriod: {
                title: "Claude limits",
                type: "enum",
                default: "auto",
                values: [
                  { name: "Both", value: "auto" },
                  { name: "Session", value: "session" },
                  { name: "Weekly", value: "weekly" },
                ],
              },
            },
          },
          supportedFamilies: [
            "systemSmall",
            "systemMedium",
            "systemLarge",
            "systemExtraLarge",
            "accessoryRectangular",
          ],
        },
        android: {
          minWidth: 250,
          minHeight: 180,
          targetCellWidth: 4,
          targetCellHeight: 3,
          resizeMode: "both",
          // The embedded layout renders before first launch; publishing replaces it with stored props.
          initialLayout: "./src/widgets/SubscriptionUsage.android.tsx",
        },
      },
      {
        name: "AgentActivity",
        displayName: "Agent Activity",
        description: "Shows the current state of active Hanzo Dev agents.",
        android: null,
        ios: {
          supportedFamilies: ["systemSmall", "systemMedium", "systemLarge", "accessoryRectangular"],
          // Embeds the layout so the gallery preview and a new widget render
          // before the app has run once; the app replaces it on publish.
          initialLayout: "./src/widgets/AgentActivityWidget.tsx",
        },
      },
    ],
  },
];

const sharingPlugin: NonNullable<ExpoConfig["plugins"]>[number] = [
  "expo-sharing",
  {
    ios: {
      // Personal Teams cannot sign App Groups or extension targets; keep local builds usable.
      enabled: !isIosPersonalTeamBuild,
      extensionBundleIdentifier: `${iosBundleIdentifier}.sharing`,
      appGroupId: `group.${iosBundleIdentifier}`,
      activationRule: {
        supportsText: true,
        supportsWebUrlWithMaxCount: 1,
        supportsImageWithMaxCount: 8,
        supportsMovieWithMaxCount: 8,
        supportsFileWithMaxCount: 8,
      },
    },
    android: {
      enabled: true,
      singleShareMimeTypes: ["*/*"],
      multipleShareMimeTypes: ["*/*"],
    },
  },
];

// Match iOS PostScript names on Android so RN and the native composer share fonts before runtime loading.

const config: ExpoConfig = {
  name: variant.appName,
  slug: "hanzo-dev",
  platforms: ["ios", "android"],
  scheme: [variant.scheme, "hanzo"],
  version: repoEnv.HANZO_DEV_MOBILE_VERSION ?? "2.3.0",
  runtimeVersion: {
    // Skip costly fingerprinting on dev launches; preview and production OTAs require matching native projects.
    policy: runtimeVersionPolicy,
  },
  orientation: "portrait",
  icon: variant.assets.appIcon,
  userInterfaceStyle: "automatic",
  updates: easProjectId
    ? {
        enabled: repoEnv.HANZO_DEV_MOBILE_UPDATES_ENABLED !== "0",
        url: `https://u.expo.dev/${easProjectId}`,
        checkAutomatically: "ON_LOAD",
        fallbackToCacheTimeout: 0,
      }
    : {
        enabled: false,
      },
  ios: {
    icon: variant.assets.iosIcon,
    supportsTablet: true,
    requireFullScreen: process.env.HANZO_SHOWCASE_CAPTURE_BUILD === "1",
    bundleIdentifier: iosBundleIdentifier,
    // Avoid Personal Team fallback: it cannot sign App Groups, Apple sign-in or push entitlements.
    appleTeamId: repoEnv.HANZO_DEV_IOS_TEAM_ID?.trim() || "Q7CPG46G4N",
    associatedDomains: [
      `applinks:${variant.relyingParty}`,
      `webcredentials:${variant.relyingParty}`,
      `applinks:${HOSTED_APP_HOST}`,
    ],
    entitlements: {
      "keychain-access-groups": [`$(AppIdentifierPrefix)${iosBundleIdentifier}`],
    },
    infoPlist: {
      UISupportedInterfaceOrientations: [
        "UIInterfaceOrientationPortrait",
        "UIInterfaceOrientationLandscapeLeft",
        "UIInterfaceOrientationLandscapeRight",
      ],
      NSAppTransportSecurity: {
        NSAllowsArbitraryLoads: true,
      },
      NSLocalNetworkUsageDescription:
        "Allow Hanzo AI to connect to local hanzod, ZAP peers, and paired Macs on your network or tailnet.",
      // Built with the iOS 27 SDK, only view controllers can hide or style the status bar.
      UIViewControllerBasedStatusBarAppearance: true,
      // Opt-in Files app, media, sensors, Siri/Shortcuts & personal data access for AI automation.
      UIFileSharingEnabled: true,
      LSSupportsOpeningDocumentsInPlace: true,
      NSDocumentsFolderUsageDescription:
        "Allow Hanzo AI to read and write files you select in your Documents folder.",
      NSPhotoLibraryUsageDescription:
        "Allow Hanzo AI to read photos and screenshots you attach for visual analysis.",
      NSPhotoLibraryAddUsageDescription:
        "Allow Hanzo AI to save generated and edited images to your photo library.",
      NSCameraUsageDescription:
        "Allow Hanzo AI to use your camera for visual questions, document capture, and QR pairing.",
      NSMicrophoneUsageDescription:
        "Allow Hanzo AI to use your microphone for voice chat and hands-free dictation.",
      NSSpeechRecognitionUsageDescription:
        "Allow Hanzo AI to transcribe your speech on-device for voice commands and dictation.",
      NSSiriUsageDescription:
        "Allow Hanzo AI to answer questions and run automations through Siri, Shortcuts, and Control Center.",
      NSCalendarsFullAccessUsageDescription:
        "Allow Hanzo AI to read and create calendar events when you enable Calendar access.",
      NSRemindersFullAccessUsageDescription:
        "Allow Hanzo AI to read and manage reminders when you enable Reminders access.",
      NSContactsUsageDescription:
        "Allow Hanzo AI to look up contacts when drafting messages or scheduling when you enable Contacts access.",
      NSLocationWhenInUseUsageDescription:
        "Allow Hanzo AI to use your current location for local context and automations when enabled.",
      // System picture in picture needs background audio to start and survive outside the app.
      UIBackgroundModes: ["audio"],
      ITSAppUsesNonExemptEncryption: false,
      // Capture builds disable multitasking because iPadOS otherwise ignores orientation requests.
      ...(process.env.HANZO_SHOWCASE_CAPTURE_BUILD === "1"
        ? {
            "UISupportedInterfaceOrientations~ipad": [
              "UIInterfaceOrientationPortrait",
              "UIInterfaceOrientationPortraitUpsideDown",
              "UIInterfaceOrientationLandscapeLeft",
              "UIInterfaceOrientationLandscapeRight",
            ],
          }
        : {}),
    },
  },
  android: {
    icon: variant.assets.appIcon,
    package: variant.androidPackage,
    ...(repoEnv.HANZO_DEV_ANDROID_GOOGLE_SERVICES_FILE
      ? { googleServicesFile: repoEnv.HANZO_DEV_ANDROID_GOOGLE_SERVICES_FILE }
      : {}),
    adaptiveIcon: {
      backgroundColor: variant.assets.androidAdaptiveBackgroundColor,
      ...(variant.assets.androidAdaptiveBackgroundImage
        ? { backgroundImage: variant.assets.androidAdaptiveBackgroundImage }
        : {}),
      foregroundImage: variant.assets.androidAdaptiveForeground,
      monochromeImage: variant.assets.androidMonochromeIcon,
    },
    // JS back dispatch needs RN's Android 16 shim and withAndroidPredictiveBackCompat on Android 13-15.
    predictiveBackGestureEnabled: true,
    // expo-sensors declares this for its pedometer, which the app does not use.
    blockedPermissions: ["android.permission.ACTIVITY_RECOGNITION"],
    intentFilters: [
      {
        action: "VIEW",
        autoVerify: true,
        data: [{ scheme: "https", host: HOSTED_APP_HOST, pathPrefix: "/threads/" }],
        category: ["BROWSABLE", "DEFAULT"],
      },
    ],
  },
  web: {
    favicon: variant.assets.appIcon,
  },
  plugins: [
    // Mods run last-in first-out; this one must come first to strip what later plugins add.
    ...(isIosPersonalTeamBuild ? ["./plugins/withoutIosPersonalTeamCapabilities.cjs"] : []),
    "expo-asset",
    [
      "expo-font",
      {
        ios: {
          fonts: [zenFonts.regular, zenFonts.medium, zenFonts.bold],
        },
        android: {
          fonts: [
            {
              fontFamily: "Zen-Regular",
              fontDefinitions: [{ path: zenFonts.regular, weight: 400 }],
            },
            {
              fontFamily: "Zen-Medium",
              fontDefinitions: [{ path: zenFonts.medium, weight: 500 }],
            },
            {
              fontFamily: "Zen-Bold",
              fontDefinitions: [{ path: zenFonts.bold, weight: 700 }],
            },
          ],
        },
      },
    ],
    [
      "expo-alternate-app-icons",
      [
        {
          name: "DevHanzo",
          ios: fromRepoRoot("assets/app-icons/dev-hanzo-1024.png"),
          android: {
            foregroundImage: fromRepoRoot("assets/app-icons/dev-hanzo-1024.png"),
            backgroundColor: variant.assets.androidAdaptiveBackgroundColor,
          },
        },
        {
          name: "Dev",
          ios: fromRepoRoot("assets/app-icons/dev-1024.png"),
          android: {
            foregroundImage: fromRepoRoot("assets/app-icons/dev-1024.png"),
            backgroundColor: variant.assets.androidAdaptiveBackgroundColor,
          },
        },
      ],
    ],
    "expo-secure-store",
    "expo-sqlite",
    ...(isIosPersonalTeamBuild
      ? [sharingPlugin]
      : ["./plugins/withShareExtensionDisplayName.cjs", sharingPlugin]),
    [
      "expo-notifications",
      {
        icon: variant.assets.androidNotificationIcon,
        color: variant.assets.androidNotificationColor,
        mode: APP_VARIANT === "development" ? "development" : "production",
      },
    ],
    "expo-web-browser",
    [
      "expo-quick-actions",
      {
        androidIcons: {
          shortcut_icon: {
            foregroundImage: variant.assets.androidAdaptiveForeground,
            backgroundColor: variant.assets.androidAdaptiveBackgroundColor,
            ...(variant.assets.androidAdaptiveBackgroundImage
              ? { backgroundImage: variant.assets.androidAdaptiveBackgroundImage }
              : {}),
          },
        },
      },
    ],
    [
      "expo-audio",
      {
        microphonePermission: "Allow Hanzo to use your microphone for voice input.",
        recordAudioAndroid: true,
        enableBackgroundPlayback: false,
        enableBackgroundRecording: false,
      },
    ],
    [
      "expo-camera",
      {
        cameraPermission: "Allow Hanzo to access your camera so you can scan pairing QR codes.",
        microphonePermission: false,
        barcodeScannerEnabled: true,
        recordAudioAndroid: false,
      },
    ],
    [
      "expo-image-picker",
      {
        photosPermission: false,
        // `false` makes this plugin add RECORD_AUDIO to blockedPermissions, breaking Android dictation.
        microphonePermission: "Allow Hanzo to use your microphone for voice input.",
      },
    ],
    [
      "expo-splash-screen",
      {
        image: variant.assets.splashIcon,
        resizeMode: "contain",
        backgroundColor: "#ffffff",
        imageWidth: 220,
        dark: {
          image: variant.assets.splashIcon,
          backgroundColor: "#0a0a0a",
        },
        android: {
          // Android 12+ masks the central two thirds of the 288dp canvas; adaptive layers avoid clipped corners.
          image: variant.assets.androidSplashIcon,
          imageWidth: 288,
          dark: { image: variant.assets.androidSplashIcon },
        },
      },
    ],
    [
      "expo-build-properties",
      {
        android: {
          minSdkVersion: 24,
          // SDK 58 lacks Kotlin 2.3's runtime-inert annotation. R8 needs explicit InputMerger constructors for widgets.
          extraProguardRules: [
            "-dontwarn kotlin.MustUseReturnValues",
            "-keep class * extends androidx.work.InputMerger { <init>(); }",
          ].join("\n"),
        },
        ios: {
          deploymentTarget: "18.0",
          // AppCheckCore 11.3+ includes Swift and needs module maps for these Objective-C dependencies.
          extraPods: [
            { name: "GoogleUtilities", modular_headers: true },
            { name: "RecaptchaInterop", modular_headers: true },
          ],
        },
      },
    ],
    "./plugins/withIosCocoaPodsUuidCache.cjs",
    // Compile out the unused pedometer to avoid an iOS motion purpose string.
    ["expo-sensors", { motionPermission: false }],
    // Mods run in reverse order; expo-widgets must create and wipe the target before assets/widgets are added.
    ...(!isIosPersonalTeamBuild
      ? ["./plugins/withAskHanzoWidgets.cjs", "./plugins/withWidgetLogoAsset.cjs", widgetsPlugin]
      : []),
    "./plugins/withAndroidNetworkSecurityConfig.cjs",
    "./plugins/withAndroidGradleHeap.cjs",
    "./plugins/withAndroidInputBackground.cjs",
    "./plugins/withAndroidModernPopupMenu.cjs",
    "./plugins/withAndroidModernAlertDialog.cjs",
    "./plugins/withAndroidPredictiveBackCompat.cjs",
    // MainActivity mods run in reverse order; extend the tablet-orientation override.
    "./plugins/withAndroidWindowDisplayMetrics.cjs",
    "./plugins/withAndroidTabletOrientation.cjs",
  ],
  extra: {
    appVariant: APP_VARIANT,
    secretsFeatureEnabled: ["1", "true"].includes(
      process.env.HANZO_DEV_FEATURE_SECRETS?.trim().toLowerCase() ?? "",
    ),
    iosPersonalTeamBuild: isIosPersonalTeamBuild,
    relay: {
      url: repoEnv.HANZO_DEV_RELAY_URL ?? null,
    },
    iam: {
      issuer: repoEnv.HANZO_DEV_IAM_ISSUER ?? null, // absent: the shared default origin
      clientId: repoEnv.HANZO_DEV_IAM_CLIENT_ID ?? "hanzo-app",
    },
    observability: {
      tracesUrl: repoEnv.EXPO_PUBLIC_OTLP_TRACES_URL ?? "https://api.axiom.co/v1/traces",
      tracesDataset: repoEnv.EXPO_PUBLIC_OTLP_TRACES_DATASET ?? null,
      tracesToken: repoEnv.EXPO_PUBLIC_OTLP_TRACES_TOKEN ?? null,
    },
    eas: {
      ...(easProjectId ? { projectId: easProjectId } : {}),
      observe: { endpointUrl: observeEndpointUrl },
    },
  },
  owner: "hanzoai",
  experiments: {
    // Metro only runs babel-plugin-react-compiler when this is set; see src/reactCompilerCoverage.test.ts.
    reactCompiler: true,
  },
};

export default config;
