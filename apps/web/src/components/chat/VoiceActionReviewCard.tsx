import { T, useGT } from "gt-react";
import { ArrowUpRightIcon, CheckCircle2Icon, PencilIcon, SparklesIcon, XIcon } from "lucide-react";
import { useState } from "react";

import { trackHanzoEvent } from "../../analytics/hanzoWebAnalytics";
import { getHanzoWebAuthController } from "../../cloud/hanzoWebAuth";
import { Button } from "../ui/button";

export interface ProposedVoiceAction {
  readonly kind: "assign" | "status" | "switch_org";
  readonly target: string;
  readonly owner: string;
  readonly summary: string;
  readonly rawText: string;
}

export interface AppliedVoiceActionResult {
  readonly target: string;
  readonly owner: string;
  readonly summary: string;
  readonly href: string;
}

const ASSIGN_PATTERN =
  /\b(?:assign|delegate|route|hand\s+off)\s+(?:this\s+|the\s+)?(?<target>[a-z0-9_#./ -]{2,48}?)\s+to\s+(?<owner>@?[a-z0-9_.-]{2,32})\b/iu;
const MOVE_STATUS_PATTERN =
  /\b(?:move|set|mark)\s+(?:this\s+|the\s+)?(?<target>[a-z0-9_#./ -]{2,48}?)\s+(?:to|as)\s+(?<status>in\s+progress|done|closed|blocked|ready|approved)\b/iu;
const SWITCH_ORG_PATTERN =
  /\b(?:switch|change)\s+(?:org|organization|workspace)\s+to\s+(?<org>[a-z0-9_-]{2,32})\b/iu;

export function parseProposedVoiceAction(rawPrompt: string): ProposedVoiceAction | null {
  const trimmed = rawPrompt.trim();
  if (trimmed.length < 8 || trimmed.length > 280) return null;

  const assignMatch = ASSIGN_PATTERN.exec(trimmed);
  if (assignMatch?.groups?.target && assignMatch.groups.owner) {
    const target = assignMatch.groups.target.trim();
    const owner = assignMatch.groups.owner.trim().replace(/^@/u, "");
    return {
      kind: "assign",
      target,
      owner,
      summary: `Assign "${target}" -> ${owner}`,
      rawText: trimmed,
    };
  }

  const moveMatch = MOVE_STATUS_PATTERN.exec(trimmed);
  if (moveMatch?.groups?.target && moveMatch.groups.status) {
    const target = moveMatch.groups.target.trim();
    const status = moveMatch.groups.status.trim();
    return {
      kind: "status",
      target,
      owner: "Current user",
      summary: `Update "${target}" status -> ${status}`,
      rawText: trimmed,
    };
  }

  const orgMatch = SWITCH_ORG_PATTERN.exec(trimmed);
  if (orgMatch?.groups?.org) {
    const org = orgMatch.groups.org.trim().toLowerCase();
    return {
      kind: "switch_org",
      target: `Organization (${org})`,
      owner: org,
      summary: `Switch active X-Org-Id -> ${org}`,
      rawText: trimmed,
    };
  }

  return null;
}

export function VoiceActionReviewCard(props: {
  readonly promptText: string;
  readonly threadHref?: string | undefined;
  readonly onRewritePrompt?: ((nextPrompt: string) => void) | undefined;
}) {
  const gt = useGT();
  const proposal = parseProposedVoiceAction(props.promptText);
  const [dismissedRaw, setDismissedRaw] = useState<string | null>(null);
  const [editing, setEditing] = useState(false);
  const [targetOverride, setTargetOverride] = useState<string | null>(null);
  const [ownerOverride, setOwnerOverride] = useState<string | null>(null);
  const [applied, setApplied] = useState<AppliedVoiceActionResult | null>(null);

  if (applied) {
    return (
      <div
        data-testid="voice-action-applied-banner"
        className="mx-3 mb-2 flex flex-wrap items-center justify-between gap-2 rounded-xl border border-emerald-500/30 bg-emerald-500/10 px-3.5 py-2 text-xs"
      >
        <div className="flex items-center gap-2">
          <CheckCircle2Icon className="size-4 shrink-0 text-emerald-500" />
          <span>
            <strong>{applied.summary}</strong> ({applied.owner})
          </span>
        </div>
        <div className="flex items-center gap-2">
          <a
            href={applied.href}
            className="inline-flex items-center gap-1 font-medium underline underline-offset-2"
          >
            <T>View updated item</T>
            <ArrowUpRightIcon className="size-3" />
          </a>
          <button
            type="button"
            aria-label={gt("Dismiss")}
            onClick={() => setApplied(null)}
            className="rounded p-1 text-muted-foreground hover:text-foreground"
          >
            <XIcon className="size-3.5" />
          </button>
        </div>
      </div>
    );
  }

  if (!proposal || dismissedRaw === proposal.rawText) return null;

  const effectiveTarget = targetOverride ?? proposal.target;
  const effectiveOwner = ownerOverride ?? proposal.owner;
  const effectiveSummary =
    proposal.kind === "assign"
      ? `Assign "${effectiveTarget}" -> ${effectiveOwner}`
      : proposal.kind === "switch_org"
        ? `Switch X-Org-Id -> ${effectiveOwner}`
        : `Update "${effectiveTarget}" (${effectiveOwner})`;

  const handleConfirm = () => {
    if (proposal.kind === "switch_org") {
      getHanzoWebAuthController().switchOrganization(effectiveOwner);
    } else if (props.onRewritePrompt && (targetOverride !== null || ownerOverride !== null)) {
      props.onRewritePrompt(`Assign ${effectiveTarget} to ${effectiveOwner}`);
    }
    const href =
      proposal.kind === "switch_org"
        ? `?org=${encodeURIComponent(effectiveOwner)}`
        : (props.threadHref ?? window.location.href);
    trackHanzoEvent("voice_action_reviewed", {
      kind: proposal.kind,
      target: effectiveTarget,
      owner: effectiveOwner,
    });
    setApplied({
      target: effectiveTarget,
      owner: effectiveOwner,
      summary: effectiveSummary,
      href,
    });
    setEditing(false);
    setTargetOverride(null);
    setOwnerOverride(null);
  };

  return (
    <div
      data-testid="voice-action-review-card"
      className="mx-3 mb-2 rounded-xl border border-border bg-muted/30 px-3.5 py-2.5 text-xs"
    >
      <div className="flex flex-wrap items-center justify-between gap-2">
        <div className="flex items-center gap-2">
          <SparklesIcon className="size-3.5 shrink-0 text-foreground" />
          <span className="font-semibold">
            <T>Voice Action Review</T>
          </span>
          <span className="rounded bg-background px-2 py-0.5 font-mono text-[11px] text-muted-foreground">
            {effectiveSummary}
          </span>
        </div>
        <div className="flex items-center gap-1.5">
          <Button variant="ghost" size="xs" onClick={() => setEditing((prev) => !prev)}>
            <PencilIcon className="size-3" />
            <T>Correct</T>
          </Button>
          <Button size="xs" onClick={handleConfirm}>
            <T>Confirm action</T>
          </Button>
          <button
            type="button"
            aria-label={gt("Dismiss voice action preview")}
            onClick={() => setDismissedRaw(proposal.rawText)}
            className="rounded p-1 text-muted-foreground hover:text-foreground"
          >
            <XIcon className="size-3.5" />
          </button>
        </div>
      </div>

      <div className="mt-1.5 flex flex-wrap items-center gap-3 text-[11px] text-muted-foreground">
        <span>
          <T>Matched target:</T> <strong className="text-foreground">{effectiveTarget}</strong>
        </span>
        <span>
          <T>Owner:</T> <strong className="text-foreground">{effectiveOwner}</strong>
        </span>
      </div>

      {editing ? (
        <div className="mt-2 grid grid-cols-1 gap-2 sm:grid-cols-2">
          <input
            type="text"
            value={effectiveTarget}
            onChange={(event) => setTargetOverride(event.currentTarget.value)}
            aria-label={gt("Matched target")}
            placeholder={gt("Target item")}
            className="rounded-lg border border-border bg-background px-2.5 py-1 text-xs focus:outline-hidden"
          />
          <input
            type="text"
            value={effectiveOwner}
            onChange={(event) => setOwnerOverride(event.currentTarget.value)}
            aria-label={gt("Owner or assignee")}
            placeholder={gt("Owner")}
            className="rounded-lg border border-border bg-background px-2.5 py-1 text-xs focus:outline-hidden"
          />
        </div>
      ) : null}
    </div>
  );
}
