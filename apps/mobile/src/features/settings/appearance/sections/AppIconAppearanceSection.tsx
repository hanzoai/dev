import {
  getAppIconName,
  setAlternateAppIcon,
  supportsAlternateIcons,
} from "expo-alternate-app-icons";
import { useGT } from "gt-react-native";
import { useState } from "react";
import { Platform, View } from "react-native";

import { ListChoiceRow as SettingsChoiceRow } from "../../../../components/ui/ListChoiceRow";
import { SettingsSection } from "../../components/SettingsSection";

// Names match the alternate icons declared in app.config.ts; null is the default `</>` with an H.
const ICONS = [
  { name: null, label: "</> H" },
  { name: "Dev", label: "</>" },
  { name: "Hanzo", label: "H" },
] as const;

export function AppIconAppearanceSection() {
  const gt = useGT();
  const [current, setCurrent] = useState<string | null>(() => getAppIconName());
  if (Platform.OS !== "ios" || !supportsAlternateIcons) return null;

  const choose = (name: string | null) => {
    setAlternateAppIcon(name as never).then(
      () => setCurrent(name),
      () => setCurrent(getAppIconName()),
    );
  };

  return (
    <SettingsSection title={gt("App icon")}>
      <View accessibilityRole="radiogroup">
        {ICONS.map((icon, index) => (
          <SettingsChoiceRow
            key={icon.label}
            label={icon.label}
            description={
              icon.name === null
                ? gt("The Dev glyph with the Hanzo H.")
                : icon.name === "Dev"
                  ? gt("The Dev glyph alone.")
                  : gt("The Hanzo H logo.")
            }
            selected={current === icon.name}
            onPress={() => choose(icon.name)}
            {...(index > 0 ? { separated: true } : {})}
          />
        ))}
      </View>
    </SettingsSection>
  );
}
