import { createFileRoute } from "@tanstack/react-router";
import { UsageSettingsPanel } from "../components/settings/UsageSettings";

export const Route = createFileRoute("/_chat/settings/usage")({ component: UsageSettingsPanel });
