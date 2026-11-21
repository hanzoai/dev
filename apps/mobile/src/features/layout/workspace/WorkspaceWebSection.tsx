import { useGT } from "gt-react-native";
import { useRef, useState } from "react";
import { Linking, Pressable, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { WebView } from "react-native-webview";

import { GlassSurface } from "../../../components/ui/GlassSurface";
import { SymbolView, type AppSymbolName } from "../../../components/ui/AppSymbol";
import { useControlTheme } from "../../../components/ui/useControlTheme";
import { webViewDecelerationRate } from "../../../lib/webViewDecelerationRate";
import { useHanzoMobileAccounts } from "../../cloud/hanzoMobileAuth";
import { useSelectWorkspaceTab } from "./useSelectWorkspaceTab";
import type { WebSection } from "./workspace-section.logic";
import {
  HANZO_APP_ORIGIN,
  isAppUrl,
  sessionScript,
  webSectionUrl,
} from "./workspace-web.logic";

function ChromeButton(props: {
  readonly label: string;
  readonly icon: AppSymbolName;
  readonly disabled?: boolean;
  readonly onPress: () => void;
}) {
  const { colors } = useControlTheme();
  return (
    <Pressable
      accessibilityRole="button"
      accessibilityLabel={props.label}
      accessibilityState={{ disabled: props.disabled === true }}
      disabled={props.disabled}
      onPress={props.onPress}
      hitSlop={4}
      className="h-11 w-14 items-center justify-center"
      style={{ opacity: props.disabled ? 0.4 : 1 }}
    >
      <SymbolView name={props.icon} size={18} tintColor={colors["--color-foreground"]} />
    </Pressable>
  );
}

/**
 * Design, Library and Tabs: hanzo.ai's route for the section in a web view, signed in as the
 * app's active account. The session script writes the @hanzo/iam keys hanzo.app reads from
 * localStorage (see workspace-web.logic.ts) before the page's own scripts run, on its origin
 * only, so there is no second sign-in. Back, reload and close are glass over the page's foot.
 */
export function WorkspaceWebSection(props: { readonly section: WebSection }) {
  const gt = useGT();
  const insets = useSafeAreaInsets();
  const selectTab = useSelectWorkspaceTab();
  const { activeAccount } = useHanzoMobileAccounts();
  const web = useRef<WebView<object>>(null);
  const [canGoBack, setCanGoBack] = useState(false);
  if (!activeAccount) return <View className="flex-1 bg-screen" />;
  const script = sessionScript({
    accessToken: activeAccount.accessToken,
    refreshToken: activeAccount.refreshToken,
    expiresAt: activeAccount.expiresAt,
  });
  return (
    <View className="flex-1 bg-screen" style={{ paddingTop: insets.top }}>
      <WebView
        // A section or account switch is a different page and a different session.
        key={`${props.section}:${activeAccount.id}`}
        ref={web}
        source={{ uri: webSectionUrl(props.section) }}
        style={{ flex: 1, backgroundColor: "transparent" }}
        contentInsetAdjustmentBehavior="never"
        decelerationRate={webViewDecelerationRate}
        originWhitelist={[HANZO_APP_ORIGIN]}
        injectedJavaScriptBeforeContentLoaded={script}
        injectedJavaScriptBeforeContentLoadedForMainFrameOnly
        sharedCookiesEnabled
        allowsBackForwardNavigationGestures
        allowsInlineMediaPlayback
        setSupportMultipleWindows={false}
        onNavigationStateChange={(state) => setCanGoBack(state.canGoBack)}
        onShouldStartLoadWithRequest={(request) => {
          if (request.url === "about:blank" || isAppUrl(request.url)) return true;
          void Linking.openURL(request.url);
          return false;
        }}
      />
      <View
        pointerEvents="box-none"
        className="absolute inset-x-0 items-center"
        style={{ bottom: Math.max(insets.bottom, 12) }}
      >
        <GlassSurface chrome="none" style={{ borderRadius: 24 }}>
          <View className="flex-row items-center">
            <ChromeButton
              label={gt("Back")}
              icon="chevron.left"
              disabled={!canGoBack}
              onPress={() => web.current?.goBack()}
            />
            <ChromeButton
              label={gt("Reload")}
              icon="arrow.clockwise"
              onPress={() => web.current?.reload()}
            />
            <ChromeButton
              label={gt("Close")}
              icon="xmark"
              onPress={() => selectTab("threads", "chat")}
            />
          </View>
        </GlassSurface>
      </View>
    </View>
  );
}
