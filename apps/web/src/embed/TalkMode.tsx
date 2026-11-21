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
    <div className="flex min-w-0 items-center gap-2">
      <button
        type="button"
        data-testid="talk-mode"
        aria-label={talk.open ? gt("End talk mode") : gt("Start talk mode")}
        aria-pressed={talk.open}
        onClick={talk.toggle}
        className="inline-flex size-7 items-center justify-center rounded-md text-muted-foreground hover:bg-accent aria-pressed:text-foreground"
      >
        <AudioLinesIcon className="size-4" />
      </button>
      {status ? (
        <span role="status" aria-live="polite" className="min-w-0 truncate text-xs text-muted-foreground">
          {status}
        </span>
      ) : null}
    </div>
  );
}
