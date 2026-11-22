import {
  flattenThemeColor,
  themeColorWithAlpha,
  type MobileThemeMode,
  type MobileThemeVariables,
} from "../lib/mobileTheme";

export interface WidgetPalette {
  background: string;
  foreground: string;
  secondary: string;
  track: string;
  danger: string;
}

export interface WidgetTheme {
  mode: MobileThemeMode;
  light: WidgetPalette;
  dark: WidgetPalette;
}

export function createWidgetTheme(
  mode: MobileThemeMode,
  palettes: Readonly<Record<"light" | "dark", MobileThemeVariables>>,
): WidgetTheme {
  const palette = (variables: MobileThemeVariables, isDark: boolean) => {
    const background = isDark ? "#000000" : variables["--color-card"];
    return {
      background,
      foreground: variables["--color-foreground"],
      secondary: variables["--color-foreground-secondary"],
      // Surface tokens can coincide; a foreground blend keeps the unfilled track visible.
      track: flattenThemeColor(
        themeColorWithAlpha(variables["--color-foreground"], 0.1),
        background,
      ),
      danger: variables["--color-danger-foreground"],
    };
  };
  return { mode, light: palette(palettes.light, false), dark: palette(palettes.dark, true) };
}
