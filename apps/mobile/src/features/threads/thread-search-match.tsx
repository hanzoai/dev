import { findSearchTokenRanges, splitSearchText } from "@hanzodev/shared/paletteSearch";
import type { EnvironmentThreadSearchMatch } from "@hanzodev/client-runtime/state/thread-search";

import { useGT } from "gt-react-native";

import { AppText as Text } from "../../components/ui/AppText";
import { cn } from "../../lib/cn";

export function ThreadSearchMatchExcerpt(props: {
  readonly match: EnvironmentThreadSearchMatch;
  readonly query: string;
  readonly selected?: boolean;
  readonly compact?: boolean;
  readonly sidebar?: boolean;
}) {
  const gt = useGT();
  const isUser = props.match.source === "user";
  const parts = splitSearchText(
    props.match.snippet,
    findSearchTokenRanges(props.match.snippet, props.query),
  );
  return (
    <Text
      className={cn(
        props.compact ? "text-sm" : "text-xs",
        props.selected
          ? "text-thread-selected-foreground-muted"
          : props.sidebar
            ? "text-drawer-foreground-muted"
            : "text-foreground-muted",
      )}
      numberOfLines={1}
    >
      <Text
        className={cn(
          props.compact ? "text-sm font-hanzo-medium" : "text-xs font-hanzo-medium",
          props.selected
            ? "text-thread-selected-foreground"
            : isUser
              ? props.sidebar
                ? "text-drawer-foreground-muted"
                : "text-foreground-secondary"
              : "text-adaptive-emerald-600-400",
        )}
      >
        {isUser
          ? gt("You:", { $context: "label before a search snippet the user wrote" })
          : gt("Agent:", { $context: "label before a search snippet the agent wrote" })}{" "}
      </Text>
      {parts.map((part) => (
        <Text
          className={cn(
            props.compact ? "text-sm" : "text-xs",
            part.highlighted && "font-hanzo-bold",
            props.selected
              ? "text-thread-selected-foreground"
              : part.highlighted
                ? props.sidebar
                  ? "text-drawer-foreground"
                  : "text-foreground"
                : props.sidebar
                  ? "text-drawer-foreground-muted"
                  : "text-foreground-muted",
          )}
          key={part.start}
        >
          {part.text}
        </Text>
      ))}
    </Text>
  );
}
