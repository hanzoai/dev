import { MOBILE_APP_DOWNLOADS } from "@hanzodev/shared/mobileApp";
import { T } from "gt-react";

import { QRCodeSvg } from "../ui/qr-code";
import {
  Dialog,
  DialogDescription,
  DialogHeader,
  DialogPanel,
  DialogPopup,
  DialogTitle,
} from "../ui/dialog";

/** A download link for the phone app. Signing in with the same account is what connects it. */
export function PhoneAppDialog({
  open,
  onOpenChange,
}: {
  readonly open: boolean;
  readonly onOpenChange: (open: boolean) => void;
}) {
  return (
    <Dialog open={open} onOpenChange={onOpenChange}>
      <DialogPopup>
        <DialogHeader>
          <DialogTitle>
            <T>Get Hanzo Dev on your phone</T>
          </DialogTitle>
          <DialogDescription>
            <T>
              Scan to download it. Sign in with the same account and your phone controls this Mac
              automatically.
            </T>
          </DialogDescription>
        </DialogHeader>
        <DialogPanel>
          <div className="flex justify-center gap-8">
            {MOBILE_APP_DOWNLOADS.map((download) => (
              <figure className="m-0 flex flex-col items-center gap-2" key={download.platform}>
                <QRCodeSvg
                  // oxlint-disable-next-line shadcn/no-restyle -- the radius clips only the white quiet zone (marginSize 2), never a module
                  className="rounded-md"
                  marginSize={2}
                  size={132}
                  title={download.platform}
                  value={download.url}
                />
                <figcaption className="text-muted-foreground text-xs">
                  {download.platform}
                </figcaption>
              </figure>
            ))}
          </div>
        </DialogPanel>
      </DialogPopup>
    </Dialog>
  );
}
