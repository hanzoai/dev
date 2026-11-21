import { useEffect } from "react";

import { m } from "~/i18n/messages";
import { msg } from "gt-react";

/**
 * Word, Excel and PowerPoint previews in the embedded build. Their engines are 75 to 108 MB
 * each, more than a host's static export can carry, so the embed asks the person to open the
 * file where the full Dev runs (the desktop app or Hanzo Dev on the web). The viewer shows
 * this as its error notice; everything else about the file (download, copy path) is unchanged.
 */
export default function OfficeUnavailable(props: { onError: (cause: unknown) => void }) {
  const { onError } = props;
  useEffect(() => {
    onError(new Error(m(msg("Open this document in Hanzo Dev on your computer to preview it."))));
  }, [onError]);
  return null;
}
