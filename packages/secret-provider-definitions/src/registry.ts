import { makeSecretProviderDefinitionRegistry } from "@hanzodev/secret-provider-core/shared/definition";
import * as OnePassword from "@hanzodev/secret-provider-onepassword/shared/definition";

export const builtInSecretProviderDefinitions = makeSecretProviderDefinitionRegistry([
  OnePassword.definition,
]);
