import { EnvironmentId } from "@hanzodev/contracts";

/** The stand-in environment of chat with no machine: it has no server to run plugins or a browser. */
export const CLOUD_ENVIRONMENT = EnvironmentId.make("cloud-chat");
