import { useGT } from "gt-react";
import { useEffect, useState } from "react";

import { Button } from "../components/ui/button";
import { Dialog, DialogDescription, DialogPopup, DialogTitle } from "../components/ui/dialog";
import { buildStatus, isGreen, label, publish, type Declared, type PublishSource } from "./publish";
import type { DevCloud } from "./runtime";

type Phase = "ask" | "sending" | "declared" | "failed";

/**
 * Publish: builds the repository on a branch and declares it in a project (`POST /v1/platform/apps`).
 * On a branch that is a review (nothing deploys until it merges); in the org's own code workspace it
 * goes live when its build does. Every state shown is one the platform reported.
 */
export function PublishDialog(props: {
  cloud: DevCloud;
  source: PublishSource | null;
  onClose: () => void;
}) {
  const { cloud, source, onClose } = props;
  const gt = useGT();
  const [phase, setPhase] = useState<Phase>("ask");
  const [out, setOut] = useState<Declared | null>(null);
  const [status, setStatus] = useState("");
  const [error, setError] = useState("");

  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- the host keeps this dialog mounted (a null `source` hides it), so a new source starts over at the ask phase; only the host could key it
  useEffect(() => {
    // oxlint-disable-next-line react/set-state-in-effect -- clears the previous source's phase, result, build status and error
    setPhase("ask");
    setOut(null);
    setStatus("");
    setError("");
    // oxlint-disable-next-line react/exhaustive-effect-dependencies -- `source` is the reset trigger, not a value the reset reads
  }, [source]);

  const id = out?.build?.id;
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- polls the platform every 5 s for the declared build's status while the dialog shows it
  useEffect(() => {
    if (!id || phase !== "declared") return;
    let live = true;
    const tick = () =>
      void buildStatus(cloud, id)
        .then((s) => live && s !== null && setStatus(s))
        .catch(() => {});
    tick();
    const every = setInterval(tick, 5000);
    return () => {
      live = false;
      clearInterval(every);
    };
  }, [cloud, id, phase]);

  if (!source) return null;
  const send = async () => {
    setPhase("sending");
    setError("");
    try {
      const declared = await publish(cloud, source);
      setOut(declared);
      setStatus(declared.build?.status ?? "");
      setPhase("declared");
    } catch (e) {
      setError((e as Error).message);
      setPhase("failed");
    }
  };
  return (
    <Dialog open onOpenChange={(open) => !open && onClose()}>
      <DialogPopup>
        <DialogTitle>{gt("Publish")}</DialogTitle>
        <DialogDescription>
          {gt("Build {repo} at {ref} and add it to a project as {name}.", {
            repo: source.repo,
            ref: source.ref,
            name: label(source.name),
          })}
        </DialogDescription>
        {out ? (
          <div role="status" className="flex flex-col gap-2 text-sm">
            {out.notice ? <p className="text-muted-foreground">{out.notice}</p> : null}
            <p>
              {out.live && isGreen(status)
                ? gt("Build {id} succeeded. It is live at the project's address.", { id: id ?? "" })
                : gt("Build {id}: {status}", { id: id ?? "", status: status || gt("building") })}
            </p>
            {out.review ? (
              <a href={out.review} target="_blank" rel="noreferrer" className="underline">
                {gt("Open the review")}
              </a>
            ) : null}
          </div>
        ) : null}
        {error ? (
          <p role="alert" className="text-sm text-destructive">
            {error}
          </p>
        ) : null}
        <div className="flex justify-end gap-2">
          <Button variant="outline" size="sm" onClick={onClose}>
            {phase === "declared" ? gt("Done") : gt("Cancel")}
          </Button>
          {phase !== "declared" ? (
            <Button size="sm" disabled={phase === "sending"} onClick={() => void send()}>
              {phase === "sending" ? gt("Publishing…") : gt("Publish")}
            </Button>
          ) : null}
        </div>
      </DialogPopup>
    </Dialog>
  );
}
