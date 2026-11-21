import { T } from "gt-react";

export function DraftHeroProjectHeading() {
  return (
    <div className="group/draft-hero relative w-full">
      <h1 className="w-full text-center font-normal text-2xl text-foreground tracking-tight sm:text-3xl [@media(max-height:28rem)]:sr-only">
        <T>Build what's next.</T>
      </h1>
    </div>
  );
}
