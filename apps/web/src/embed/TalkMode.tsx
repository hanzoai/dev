import { useTalk } from "@hanzo/voice";
import { AudioLinesIcon } from "lucide-react";
import { useGT } from "gt-react";

import type { DevCloud } from "./runtime";

/**
 * Talk mode: a hands-free spoken conversation on the gateway's realtime socket (`/v1/voice`),
 * the same one the old Chat's waveform button opened. One press opens the microphone, the reply
 * is spoken back with its words beside it, and a second press hangs up. Dev's own dictation
 * (the composer's mic) is unchanged; this is the waveform.
 */
export function TalkMode({ cloud }: { cloud: DevCloud }) {
  const gt = useGT();
  const talk = useTalk({
    baseUrl: cloud.api,
    token: () => cloud.token(),
    ...(cloud.org ? { org: cloud.org } : {}),
  });
  const status = talk.refusal
    ? gt("Talk mode is not available right now.")
    : talk.open
      ? talk.reply
        ? talk.reply
        : talk.state === "listening"
          ? gt("Listening. Speak, and Hanzo answers aloud.")
          : gt("Connecting…")
      : null;
  return (
    <div className="inline-flex min-w-0 items-center gap-1.5">
      <button
        type="button"
        data-testid="talk-mode"
        aria-label={talk.open ? gt("End talk mode") : gt("Start talk mode")}
        aria-pressed={talk.open}
        title={talk.open ? gt("End voice conversation") : gt("Voice conversation")}
        onClick={talk.toggle}
        className="inline-flex size-7 shrink-0 cursor-pointer items-center justify-center rounded-full text-muted-foreground transition-colors hover:bg-accent hover:text-foreground aria-pressed:bg-foreground/15 aria-pressed:text-foreground"
      >
        <AudioLinesIcon className="size-4" />
      </button>
      {status ? (
        <span role="status" aria-live="polite" className="max-w-44 min-w-0 truncate text-xs text-muted-foreground">
          {status}
        </span>
      ) : null}
    </div>
  );
}
