import { T } from "gt-react";
import { cn } from "~/lib/utils";

import { CommandBlock } from "./CommandBlock";

/**
 * Setup steps for another computer: install `hanzo`, then run `command`. Both installers are shown
 * because the browser showing this is often not the computer being set up.
 */
export function InstallHanzoCommands({
  command,
  className,
}: {
  readonly command: string;
  readonly className?: string;
}) {
  return (
    <div className={cn("space-y-2", className)}>
      <p className="text-xs text-muted-foreground">macOS, Linux</p>
      <CommandBlock command="curl -fsSL https://dev.hanzo.ai/install.sh | sh" />
      <p className="text-xs text-muted-foreground">Windows (PowerShell)</p>
      <CommandBlock command="irm https://dev.hanzo.ai/install.ps1 | iex" />
      <p className="text-xs text-muted-foreground">
        <T>Then</T>
      </p>
      <CommandBlock command={command} />
      <p className="text-xs text-muted-foreground">
        <T>
          Skip the install if you already have <code className="font-mono">hanzo</code>. If{" "}
          <code className="font-mono">hanzo</code> is not found after installing, add the folder the
          installer names to your PATH.
        </T>
      </p>
    </div>
  );
}
