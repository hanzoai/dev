import { createFileRoute } from "@tanstack/react-router";

import { LinkedDesktopSettings } from "../components/settings/LinkedDesktopSettings";

export const Route = createFileRoute("/_chat/settings/hanzo")({
  component: LinkedDesktopSettings,
});
