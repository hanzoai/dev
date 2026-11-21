import { createBrowserSourceAtoms } from "@hanzodev/client-runtime/state/browserSources";

import { connectionAtomRuntime } from "../connection/runtime";

export const browserSourceEnvironment = createBrowserSourceAtoms(connectionAtomRuntime);
