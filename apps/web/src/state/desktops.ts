import { createDesktopEnvironmentAtoms } from "@hanzodev/client-runtime/state/desktops";

import { connectionAtomRuntime } from "../connection/runtime";

export const desktopEnvironment = createDesktopEnvironmentAtoms(connectionAtomRuntime);
