import {
  buildConnectAuthorizeUrl,
  connectLoopbackRedirectUri,
  CONNECT_OAUTH_SCOPES,
  type ConnectAuthorizeRequest,
} from "@hanzodev/shared/connectAuth";

import { isHostedStaticApp } from "../hostedPairing";
import { hasCloudPublicConfig, resolveCloudPublicConfig } from "./publicConfig";

export function connectCliAuthRoutesEnabled(): boolean {
  return isHostedStaticApp() && hasCloudPublicConfig();
}

export function buildConnectCliAuthorizeUrl(request: ConnectAuthorizeRequest): string | null {
  const { iamIssuer, iamClientId } = resolveCloudPublicConfig();
  if (!iamIssuer || !iamClientId) {
    return null;
  }
  return buildConnectAuthorizeUrl({
    authorizationEndpoint: `${iamIssuer}/oauth/authorize`,
    clientId: iamClientId,
    redirectUri: connectLoopbackRedirectUri(request.loopbackPort),
    scopes: CONNECT_OAUTH_SCOPES,
    state: request.state,
    challenge: request.challenge,
  });
}

/** Force a new document: fragment revisits do not reload, and the IAM session may not update in place. */
export function connectCliSignInRedirectUrl(
  request: ConnectAuthorizeRequest,
  currentHref: string,
): string {
  return buildConnectCliAuthorizeUrl(request) ?? currentHref;
}
