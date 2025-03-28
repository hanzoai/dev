import HanzoLogo from "#/assets/branding/all-hands-logo.svg?react";
import { TooltipButton } from "./tooltip-button";

interface HanzoLogoButtonProps {
  onClick: () => void;
}

export function HanzoLogoButton({ onClick }: HanzoLogoButtonProps) {
  return (
    <TooltipButton
      tooltip="Hanzo AI"
      ariaLabel="Hanzo Logo"
      onClick={onClick}
    >
      <HanzoLogo width={34} height={34} />
    </TooltipButton>
  );
}
