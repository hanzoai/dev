import { builtInSecretProviderDefinitions } from "@hanzodev/secret-provider-definitions/registry";
import * as OnePasswordDriver from "@hanzodev/secret-provider-onepassword/server/driver";

/** In the shared registry's order. */
export const BUILT_IN_SECRET_PROVIDER_DRIVER_LIST = [OnePasswordDriver.driver] as const;

export const secretProviderDefinitions = builtInSecretProviderDefinitions;
