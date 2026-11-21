import { SquarePenIcon } from "lucide-react";
import type { ComponentProps } from "react";

import { Button } from "./ui/button";

/** Round control in the phone header: the Hanzo mobile app's drawer and new-chat buttons. */
export function MobileCircleButton(props: Omit<ComponentProps<typeof Button>, "variant" | "size">) {
  return (
    <Button
      data-slot="mobile-circle-button"
      className="pointer-events-auto size-10"
      size="icon-lg"
      variant="glass"
      {...props}
    />
  );
}

/** Two lines, the short one under the long one: the drawer's mark. */
export function MobileMenuIcon() {
  return (
    <svg
      aria-hidden="true"
      className="size-5"
      fill="none"
      stroke="currentColor"
      strokeLinecap="round"
      strokeWidth={2}
      viewBox="0 0 24 24"
    >
      <path d="M4 8h16" />
      <path d="M4 16h10" />
    </svg>
  );
}

export function MobileNewChatIcon() {
  return <SquarePenIcon className="size-5" />;
}
