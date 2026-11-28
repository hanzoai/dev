import type { ModelSelection, ProviderDriverKind } from "@hanzodev/contracts";
import { useGT, useMessages } from "gt-react-native";
import { ensoModelGap, ensoRingDot, isEnsoModel } from "@hanzodev/client-runtime/enso-ring";
import {
  buildTraitsTriggerDisplay,
  getDescriptorStringValue,
  getPromptControlledTraits,
  isReasoningControl,
} from "@hanzodev/client-runtime/model-traits";
import type { ModelOption } from "../lib/modelOptions";
import { resolveProviderOptionDescriptors } from "../lib/providerOptions";
import { AppText as Text } from "./ui/AppText";
import { ComposerInlineControl } from "./ComposerToolbar";
import { ProviderIcon, ProviderInstanceIcon, providerInstanceNameLabel } from "./ProviderIcon";

export function ComposerModelControl(props: {
  readonly modelOption: ModelOption | null;
  readonly selection: ModelSelection | null;
  readonly provider: ProviderDriverKind | undefined;
  readonly reportedSelection?: ModelSelection | null | undefined;
  readonly prompt: string;
  readonly surfaceColor: string;
  readonly disabled?: boolean;
  readonly size?: "regular" | "large";
  readonly iconOnly?: boolean;
  readonly onPress: () => void;
}) {
  const gt = useGT();
  const m = useMessages();
  const descriptors = resolveProviderOptionDescriptors({
    capabilities: props.modelOption?.capabilities,
    selections: props.selection?.options,
  });
  const promptTraits = getPromptControlledTraits(descriptors, props.prompt);
  const summary = props.provider
    ? buildTraitsTriggerDisplay(
        {
          descriptors,
          primarySelectDescriptorId: promptTraits.primarySelectDescriptor?.id ?? null,
          ultrathinkPromptControlled: promptTraits.ultrathinkPromptControlled,
          modelSelection: props.selection,
          reportedModelSelection: props.reportedSelection,
        },
        m,
      ).label
    : "";
  const ensoModel =
    props.provider === "hanzo" && props.selection && isEnsoModel(props.selection.model)
      ? props.selection.model
      : null;
  const ensoSelect = ensoModel
    ? descriptors.find(
        (descriptor): descriptor is Extract<typeof descriptor, { type: "select" }> =>
          descriptor.type === "select" && isReasoningControl(descriptor),
      )
    : undefined;
  const ensoEffort = ensoSelect
    ? getDescriptorStringValue(ensoSelect, props.selection, props.reportedSelection)
    : null;
  const ensoEffortLabel = ensoSelect?.options.find((option) => option.id === ensoEffort)?.label;
  const ensoDetail = ensoSelect
    ? ensoEffortLabel
      ? gt("{effort} effort", { effort: ensoEffortLabel.toLowerCase() })
      : gt("Auto effort")
    : null;
  const effortSummary = ensoSelect ? "" : summary;
  const badge = props.modelOption?.providerBadge;
  const label = props.modelOption?.label ?? props.selection?.model ?? gt("Choose model");
  const providerNameLabel = badge
    ? providerInstanceNameLabel({
        provider: props.modelOption?.providerDriver,
        iconUrl: props.modelOption?.providerIconUrl,
        displayName: badge.displayName,
        showBadge: badge.showBadge,
      })
    : null;
  const shortLabel = props.modelOption?.shortName ?? label;
  return (
    <ComposerInlineControl
      accessibilityLabel={[
        gt("Model and reasoning settings"),
        label,
        badge?.showBadge ? badge.displayName : null,
        ensoDetail ?? summary,
      ]
        .filter(Boolean)
        .join(", ")}
      size={props.size}
      disabled={props.disabled}
      emphasized
      renderIcon={(size) =>
        ensoModel ? (
          <ProviderIcon
            provider="enso"
            ensoGap={ensoModelGap(ensoModel)}
            ensoDot={ensoRingDot(ensoEffort)}
            size={size}
          />
        ) : badge?.showBadge ? (
          <ProviderInstanceIcon
            iconUrl={props.modelOption?.providerIconUrl}
            provider={props.modelOption?.providerDriver}
            displayName={badge.displayName}
            accentColor={badge.accentColor}
            showBadge
            surfaceColor={props.surfaceColor}
            size={size}
          />
        ) : (
          <ProviderIcon
            iconUrl={props.modelOption?.providerIconUrl}
            provider={props.modelOption?.providerDriver}
            size={size}
          />
        )
      }
      label={providerNameLabel ?? shortLabel}
      labelEllipsizeMode={providerNameLabel ? "middle" : undefined}
      iconOnly={props.iconOnly}
      suffixWidth={providerNameLabel ? 96 : undefined}
      suffix={
        providerNameLabel ? (
          <Text
            className={
              props.size === "large"
                ? "text-sm text-foreground-muted"
                : "text-xs text-foreground-muted"
            }
            numberOfLines={1}
          >
            {shortLabel}
            {effortSummary ? ` · ${effortSummary}` : ""}
          </Text>
        ) : effortSummary ? (
          <Text
            className={
              props.size === "large"
                ? "text-sm text-foreground-muted"
                : "text-xs text-foreground-muted"
            }
            numberOfLines={1}
          >
            {effortSummary}
          </Text>
        ) : undefined
      }
      maxWidth="100%"
      onPress={props.onPress}
    />
  );
}
