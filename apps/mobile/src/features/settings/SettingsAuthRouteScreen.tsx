import { useGT } from "gt-react-native";
import { useNavigation } from "@react-navigation/native";
import { useState } from "react";
import { Pressable, View } from "react-native";
import { useSafeAreaInsets } from "react-native-safe-area-context";

import { AppBar } from "../../components/ui/AppBar";
import { AppText as Text, AppTextInput } from "../../components/ui/AppText";
import { Button } from "../../components/ui/Button";
import * as Dialog from "../../components/ui/Dialog";
import { ScreenScrollView as ScrollView } from "../../components/ui/ScreenScrollView";
import { HanzoWordmark } from "../../components/HanzoWordmark";
import { HanzoConnectProfilePage } from "../cloud/HanzoConnectProfilePage";
import {
  type HanzoIamBrandId,
  useHanzoMobileAccounts,
} from "../cloud/hanzoMobileAuth";
import { hasCloudPublicConfig } from "../cloud/publicConfig";

export function SettingsAuthRouteScreen() {
  return hasCloudPublicConfig() ? <ConfiguredSettingsAuthRouteScreen /> : null;
}

function ConfiguredSettingsAuthRouteScreen() {
  const gt = useGT();
  const navigation = useNavigation();
  const insets = useSafeAreaInsets();
  const {
    isLoaded,
    activeAccount,
    accounts,
    brands,
    signInWithBrand,
    signInWithToken,
    switchAccount,
    removeAccount,
    signOutAll,
  } = useHanzoMobileAccounts();

  const [selectedBrand, setSelectedBrand] = useState<HanzoIamBrandId>("hanzo");
  const [busy, setBusy] = useState(false);
  const [showTokenEntry, setShowTokenEntry] = useState(false);
  const [tokenInput, setTokenInput] = useState("");

  const handleClose = () => navigation.goBack();

  const handleBrandSignIn = (brandId: HanzoIamBrandId) => {
    if (busy) return;
    setBusy(true);
    void signInWithBrand(brandId)
      .catch((error: unknown) => {
        const msg = error instanceof Error ? error.message : gt("Could not complete Hanzo sign-in.");
        if (!/cancel/i.test(msg)) {
          Dialog.alert(gt("Sign-in failed"), msg);
        }
      })
      .finally(() => setBusy(false));
  };

  const handleTokenSignIn = () => {
    if (busy || !tokenInput.trim()) return;
    setBusy(true);
    void signInWithToken({ token: tokenInput, brand: selectedBrand })
      .then(() => {
        setTokenInput("");
        setShowTokenEntry(false);
      })
      .catch((error: unknown) => {
        const msg = error instanceof Error ? error.message : gt("Invalid token.");
        Dialog.alert(gt("Sign-in failed"), msg);
      })
      .finally(() => setBusy(false));
  };

  return (
    <View collapsable={false} className="flex-1 bg-sheet">
      <AppBar
        title={activeAccount ? gt("Hanzo Account") : gt("Sign in to Hanzo")}
        onClose={handleClose}
      />
      {!isLoaded ? null : (
        <ScrollView
          nestedScrollEnabled
          alwaysBounceVertical
          contentInsetAdjustmentBehavior="automatic"
          showsVerticalScrollIndicator={false}
          className="flex-1"
          contentContainerStyle={{
            gap: 18,
            paddingHorizontal: 20,
            paddingTop: 18,
            paddingBottom: Math.max(insets.bottom, 20) + 20,
          }}
        >
          <View className="items-center gap-3 rounded-[24px] bg-grouped-card p-6">
            <HanzoWordmark height={36} colorClassName="text-foreground" />
            <Text className="text-lg font-hanzo-bold text-foreground">
              {activeAccount
                ? (activeAccount.displayName ?? activeAccount.id)
                : gt("Unified Remote Control")}
            </Text>
            <Text className="text-center text-sm text-foreground-muted">
              {activeAccount
                ? `${activeAccount.id} · ${activeAccount.issuer.replace(/^https?:\/\//, "")}`
                : gt("Sign in with Hanzo ID to control every Desktop, CLI, and Cloud sandbox across your accounts.")}
            </Text>
          </View>

          {accounts.length > 0 ? (
            <View className="gap-2 rounded-[24px] bg-grouped-card p-4">
              <Text className="px-2 text-xs font-hanzo-bold uppercase text-foreground-muted">
                {gt("Connected Accounts")}
              </Text>
              {accounts.map((account) => {
                const isActive = account.id === activeAccount?.id;
                return (
                  <View
                    key={account.id}
                    className="flex-row items-center justify-between rounded-2xl px-3 py-3"
                  >
                    <Pressable
                      className="flex-1 gap-0.5"
                      onPress={() => {
                        void switchAccount(account.id);
                      }}
                    >
                      <Text className="text-base font-medium text-foreground">
                        {account.displayName ?? account.id}
                      </Text>
                      <Text className="text-xs text-foreground-muted">
                        {account.email ? `${account.id} · ${account.email}` : account.id}
                      </Text>
                    </Pressable>
                    <View className="flex-row items-center gap-2">
                      {isActive ? (
                        <View className="rounded-full bg-subtle px-2.5 py-1">
                          <Text className="text-2xs font-hanzo-bold uppercase text-foreground">
                            {gt("Active")}
                          </Text>
                        </View>
                      ) : (
                        <Button
                          label={gt("Switch")}
                          tone="secondary"
                          onPress={() => {
                            void switchAccount(account.id);
                          }}
                        />
                      )}
                      <Button
                        label={gt("Remove")}
                        tone="text"
                        onPress={() => {
                          void removeAccount(account.id);
                        }}
                      />
                    </View>
                  </View>
                );
              })}
            </View>
          ) : null}

          <View className="gap-3 rounded-[24px] bg-grouped-card p-5">
            <Text className="text-xs font-hanzo-bold uppercase text-foreground-muted">
              {accounts.length > 0 ? gt("Add Another Account") : gt("Choose Identity Network")}
            </Text>
            <View className="flex-row flex-wrap gap-2">
              {brands.map((brand) => {
                const selected = selectedBrand === brand.id;
                return (
                  <Pressable
                    key={brand.id}
                    onPress={() => setSelectedBrand(brand.id)}
                    className={`rounded-full px-3.5 py-2 ${selected ? "bg-foreground" : "bg-subtle"}`}
                  >
                    <Text
                      className={`text-xs font-hanzo-bold ${selected ? "text-background" : "text-foreground"}`}
                    >
                      {brand.label}
                    </Text>
                  </Pressable>
                );
              })}
            </View>
            <Button
              label={
                busy
                  ? gt("Waiting for browser return...")
                  : gt("Continue with {brand} ID", {
                      brand: brands.find((b) => b.id === selectedBrand)?.label ?? "Hanzo",
                    })
              }
              disabled={busy}
              onPress={() => handleBrandSignIn(selectedBrand)}
            />
            {busy ? (
              <View className="flex-row flex-wrap items-center justify-between gap-2 rounded-2xl bg-subtle px-3.5 py-2.5">
                <Text className="flex-1 text-xs text-foreground-muted">
                  {gt("Closed the browser tab? Cancel or reopen sign-in.")}
                </Text>
                <Button
                  label={gt("Cancel")}
                  tone="secondary"
                  onPress={() => setBusy(false)}
                />
              </View>
            ) : null}
            <Button
              label={showTokenEntry ? gt("Hide API key / token") : gt("Use API key or CLI token")}
              tone="text"
              onPress={() => setShowTokenEntry((prev) => !prev)}
            />
            {showTokenEntry ? (
              <View className="gap-3 pt-1">
                <AppTextInput
                  value={tokenInput}
                  onChangeText={setTokenInput}
                  autoCapitalize="none"
                  autoCorrect={false}
                  secureTextEntry
                  placeholder={gt("Paste Hanzo IAM access token or API key")}
                  className="rounded-2xl bg-subtle px-4 py-3 text-sm text-foreground"
                />
                <Button
                  label={gt("Connect with token")}
                  disabled={busy || !tokenInput.trim()}
                  onPress={handleTokenSignIn}
                />
              </View>
            ) : null}
          </View>

          {activeAccount ? (
            <View className="overflow-hidden rounded-[24px] bg-grouped-card">
              <HanzoConnectProfilePage />
            </View>
          ) : null}

          {accounts.length > 1 ? (
            <Button
              label={gt("Sign out of all accounts")}
              tone="text"
              onPress={() => {
                void signOutAll();
              }}
            />
          ) : null}
        </ScrollView>
      )}
    </View>
  );
}
