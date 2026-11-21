import { T } from "gt-react";
import { CloudIcon, LaptopIcon } from "lucide-react";

import { Button } from "../components/ui/button";
import { Empty, EmptyDescription, EmptyHeader, EmptyTitle } from "../components/ui/empty";

/**
 * Dev's own state for a person with no runtime yet: the two ways to get one. The host owns
 * what each does (lease a cloud session, link a machine), so it passes both.
 */
export function DevStart({
  onCloud,
  onLink,
  className,
}: {
  onCloud: () => void;
  onLink: () => void;
  className?: string;
}) {
  return (
    <Empty className={className} data-dev-start="">
      <div className="w-full max-w-lg px-8 py-12">
        <EmptyHeader className="max-w-none">
          <EmptyTitle>
            <T>Build what's next.</T>
          </EmptyTitle>
          <EmptyDescription>
            <T>Start a session in the cloud, or link your own machine.</T>
          </EmptyDescription>
        </EmptyHeader>
        <div className="mt-6 flex flex-wrap justify-center gap-2">
          <Button onClick={onCloud}>
            <CloudIcon />
            <T>Start a cloud session</T>
          </Button>
          <Button variant="outline" onClick={onLink}>
            <LaptopIcon />
            <T>Link my machine</T>
          </Button>
        </div>
      </div>
    </Empty>
  );
}
