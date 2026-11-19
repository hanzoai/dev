import { T } from "gt-react";
import type { ReactNode } from "react";

export function DraftHeroProjectHeading({
  children,
  projectName: _projectName,
}: {
  children: ReactNode;
  projectName: string;
}) {
  return (
    <div className="group/draft-hero relative w-full">
      <h1 className="flex w-full flex-col items-center justify-center gap-2 text-center font-normal text-2xl text-foreground tracking-tight sm:text-3xl">
        <span className="max-w-full [@media(max-height:28rem)]:sr-only">
          <T>Build what's next.</T>
        </span>
        <span className="flex min-w-0 max-w-full items-baseline justify-center text-base tracking-normal">
          <span className="min-w-0">{children}</span>
        </span>
      </h1>
    </div>
  );
}
