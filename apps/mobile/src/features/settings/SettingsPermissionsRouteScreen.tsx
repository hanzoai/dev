import { useCallback, useEffect, useState } from "react";
import { Linking, Platform, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";
import { AudioModule } from "expo-audio";
import { Camera } from "expo-camera";
import * as DocumentPicker from "expo-document-picker";
import * as ImagePicker from "expo-image-picker";
import * as Notifications from "expo-notifications";
import { useGT } from "gt-react-native";

import * as Dialog from "../../components/ui/Dialog";
import { ScreenScrollView as ScrollView } from "../../components/ui/ScreenScrollView";
import { useNativeColumnLayoutMetrics } from "../../native/native-layout-metrics";
import { SettingsRow } from "./components/SettingsRow";
import { SettingsScreen } from "./components/SettingsScreen";
import { SettingsSection } from "./components/SettingsSection";

type GrantState = "granted" | "denied" | "undetermined" | "checking";

function formatGrant(state: GrantState, gt: (s: string) => string): string {
  switch (state) {
    case "granted":
      return gt("Enabled");
    case "denied":
      return gt("Open Settings");
    case "undetermined":
      return gt("Enable");
    default:
      return gt("Checking");
  }
}

/**
 * Opt-in AI Permissions & Automation hub.
 * Gives the user one screen to grant Hanzo AI access to camera, photos, microphone,
 * files/folders, notifications, Siri/Shortcuts automation, Control Center controls,
 * and system data (Calendar, Reminders, Contacts, Location, Local Network).
 */
export function SettingsPermissionsRouteScreen() {
  const gt = useGT();
  const insets = useSafeAreaInsets();
  const bottomSafeArea = useNativeColumnLayoutMetrics()?.safeArea.bottom ?? insets.bottom;

  const [camera, setCamera] = useState<GrantState>("checking");
  const [photos, setPhotos] = useState<GrantState>("checking");
  const [mic, setMic] = useState<GrantState>("checking");
  const [notifications, setNotifications] = useState<GrantState>("checking");
  const [grantedFilesCount, setGrantedFilesCount] = useState<number>(0);

  const refresh = useCallback(async () => {
    const [camRes, photoRes, micRes, notifRes] = await Promise.all([
      Camera.getCameraPermissionsAsync().catch(() => null),
      ImagePicker.getMediaLibraryPermissionsAsync().catch(() => null),
      AudioModule.getRecordingPermissionsAsync().catch(() => null),
      Notifications.getPermissionsAsync().catch(() => null),
    ]);
    setCamera(camRes?.granted ? "granted" : camRes?.canAskAgain === false ? "denied" : "undetermined");
    setPhotos(
      photoRes?.granted ? "granted" : photoRes?.canAskAgain === false ? "denied" : "undetermined",
    );
    setMic(micRes?.granted ? "granted" : micRes?.canAskAgain === false ? "denied" : "undetermined");
    setNotifications(
      notifRes?.granted ? "granted" : notifRes?.canAskAgain === false ? "denied" : "undetermined",
    );
  }, []);

  useEffect(() => {
    void refresh();
  }, [refresh]);

  const openSystemSettings = useCallback(() => {
    void Linking.openSettings().catch(() => {});
  }, []);

  const requestCamera = useCallback(async () => {
    if (camera === "denied") {
      openSystemSettings();
      return;
    }
    const res = await Camera.requestCameraPermissionsAsync().catch(() => null);
    setCamera(res?.granted ? "granted" : "denied");
  }, [camera, openSystemSettings]);

  const requestPhotos = useCallback(async () => {
    if (photos === "denied") {
      openSystemSettings();
      return;
    }
    const res = await ImagePicker.requestMediaLibraryPermissionsAsync().catch(() => null);
    setPhotos(res?.granted ? "granted" : "denied");
  }, [photos, openSystemSettings]);

  const requestMic = useCallback(async () => {
    if (mic === "denied") {
      openSystemSettings();
      return;
    }
    const res = await AudioModule.requestRecordingPermissionsAsync().catch(() => null);
    setMic(res?.granted ? "granted" : "denied");
  }, [mic, openSystemSettings]);

  const requestNotifications = useCallback(async () => {
    if (notifications === "denied") {
      openSystemSettings();
      return;
    }
    const res = await Notifications.requestPermissionsAsync().catch(() => null);
    setNotifications(res?.granted ? "granted" : "denied");
  }, [notifications, openSystemSettings]);

  const grantFilesAccess = useCallback(async () => {
    try {
      const result = await DocumentPicker.getDocumentAsync({
        multiple: true,
        copyToCacheDirectory: true,
        type: "*/*",
      });
      if (!result.canceled && result.assets.length > 0) {
        setGrantedFilesCount((prev) => prev + result.assets.length);
        Dialog.alert(
          gt("Files access ready"),
          gt(
            "Selected files are now accessible to Hanzo AI. You can also place files directly in On My iPhone → Hanzo in the iOS Files app.",
          ),
        );
      }
    } catch {
      openSystemSettings();
    }
  }, [gt, openSystemSettings]);

  const openShortcutsApp = useCallback(() => {
    void Linking.openURL("shortcuts://").catch(() => {
      openSystemSettings();
    });
  }, [openSystemSettings]);

  const enableAllCore = useCallback(async () => {
    await Promise.all([
      Camera.requestCameraPermissionsAsync().catch(() => null),
      ImagePicker.requestMediaLibraryPermissionsAsync().catch(() => null),
      AudioModule.requestRecordingPermissionsAsync().catch(() => null),
      Notifications.requestPermissionsAsync().catch(() => null),
    ]);
    await refresh();
  }, [refresh]);

  return (
    <View className="flex-1 bg-screen">
      <SettingsScreen title={gt("AI Permissions & Automation")}>
        <ScrollView
          contentInsetAdjustmentBehavior="automatic"
          showsVerticalScrollIndicator={false}
          className="flex-1 bg-screen"
          contentContainerClassName="gap-4 px-5 pt-4"
          contentContainerStyle={{
            paddingBottom: Platform.OS === "ios" ? Math.max(bottomSafeArea, 24) : 24,
          }}
        >
          <SettingsSection
            title={gt("Quick Setup")}
            footer={gt(
              "Enable core device permissions so Hanzo AI can see via Camera & Photos, hear via Voice, and deliver live agent notifications.",
            )}
          >
            <SettingsRow
              icon="checkmark.shield"
              label={gt("Enable All Core AI Permissions")}
              value={
                camera === "granted" &&
                photos === "granted" &&
                mic === "granted" &&
                notifications === "granted"
                  ? gt("All Enabled")
                  : gt("Enable")
              }
              valuePosition="trailing"
              onPress={() => void enableAllCore()}
            />
            <SettingsRow
              icon="gearshape"
              label={gt("Open iOS Settings for Hanzo")}
              value={gt("Manage All")}
              valuePosition="trailing"
              onPress={openSystemSettings}
            />
          </SettingsSection>

          <SettingsSection
            title={gt("Files, Media & Sensors")}
            footer={gt(
              "Hanzo exposes its workspace in the iOS Files app (On My iPhone → Hanzo) and supports in-place document access.",
            )}
          >
            <SettingsRow
              icon="folder"
              label={gt("Files & Documents")}
              value={
                grantedFilesCount > 0 ? `${grantedFilesCount} ${gt("granted")}` : gt("Grant Access")
              }
              valuePosition="trailing"
              onPress={() => void grantFilesAccess()}
            />
            <SettingsRow
              icon="camera"
              label={gt("Camera & Vision")}
              value={formatGrant(camera, gt)}
              valuePosition="trailing"
              onPress={() => void requestCamera()}
            />
            <SettingsRow
              icon="photo"
              label={gt("Photo Library")}
              value={formatGrant(photos, gt)}
              valuePosition="trailing"
              onPress={() => void requestPhotos()}
            />
            <SettingsRow
              icon="mic"
              label={gt("Microphone & Speech")}
              value={formatGrant(mic, gt)}
              valuePosition="trailing"
              onPress={() => void requestMic()}
            />
            <SettingsRow
              icon="bell.badge"
              label={gt("Notifications & Live Activities")}
              value={formatGrant(notifications, gt)}
              valuePosition="trailing"
              onPress={() => void requestNotifications()}
            />
          </SettingsSection>

          <SettingsSection
            title={gt("Personal Data & System Access")}
            footer={gt(
              "Opt in via iOS Settings to allow Hanzo AI automations to read/create Calendar events, Reminders, Contacts, Location context, and local ZAP/hanzod devices.",
            )}
          >
            <SettingsRow
              icon="calendar"
              label={gt("Calendar, Reminders & Contacts")}
              value={gt("iOS Settings")}
              valuePosition="trailing"
              onPress={openSystemSettings}
            />
            <SettingsRow
              icon="location"
              label={gt("Location & Local Network (ZAP)")}
              value={gt("iOS Settings")}
              valuePosition="trailing"
              onPress={openSystemSettings}
            />
          </SettingsSection>

          <SettingsSection
            title={gt("Siri, Shortcuts, Widgets & Control Center")}
            footer={gt(
              "Swipe down for Control Center → Edit → Add Control → Hanzo (Ask Hanzo, Voice, Vision, Dev, or Quick Control). You can also bind Hanzo to the iPhone Action Button or Lock Screen.",
            )}
          >
            <SettingsRow
              icon="wand.and.stars"
              label={gt("Apple Shortcuts & Automations")}
              value={gt("Open Shortcuts")}
              valuePosition="trailing"
              onPress={openShortcutsApp}
            />
            <SettingsRow
              icon="sparkles"
              label={gt("Siri Voice Triggers")}
              value={gt("“Ask Hanzo”")}
              valuePosition="trailing"
              onPress={openSystemSettings}
            />
          </SettingsSection>
        </ScrollView>
      </SettingsScreen>
    </View>
  );
}
