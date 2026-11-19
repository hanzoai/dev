import { T } from "gt-react";
import type { ReactNode } from "react";

export function DraftHeroProjectHeading({
  children: _children,
  projectName: _projectName,
}: {
  children: ReactNode;
  projectName: string;
}) {
  return (
    <div className="group/draft-hero relative w-full">
      <h1 className="flex w-full items-center justify-center text-center font-normal text-2xl text-foreground tracking-tight sm:text-3xl">
        <span className="max-w-full [@media(max-height:28rem)]:sr-only">
          <T>Build what's next.</T>
        </span>
      </h1>
    </div>
  );
}
