import {
  buildConnectClerkAuthorizeUrl,
  connectLoopbackRedirectUri,
  CONNECT_OAUTH_SCOPES,
  type ConnectAuthorizeRequest,
} from "@hanzodev/shared/connectAuth";
import { clerkFrontendApiUrlFromPublishableKey } from "@hanzodev/shared/relayAuth";

import { isHostedStaticApp } from "../hostedPairing";
import { hasCloudPublicConfig, resolveCloudPublicConfig, trimNonEmpty } from "./publicConfig";

function resolveConnectCliOAuthClientId(): string | null {
  return (
    trimNonEmpty(import.meta.env.VITE_CLERK_CLI_OAUTH_CLIENT_ID as string | undefined) ??
    resolveCloudPublicConfig().iamClientId
  );
}

function hasConnectCliAuthConfig(): boolean {
  const config = resolveCloudPublicConfig();
  return Boolean(
    (config.clerkPublishableKey && resolveConnectCliOAuthClientId()) ||
    (config.iamIssuer && config.iamClientId),
  );
}

export function connectCliAuthRoutesEnabled(): boolean {
  return isHostedStaticApp() && hasCloudPublicConfig() && hasConnectCliAuthConfig();
}

export function buildConnectCliClerkAuthorizeUrl(request: ConnectAuthorizeRequest): string | null {
  const { clerkPublishableKey, iamIssuer, iamClientId } = resolveCloudPublicConfig();
  const clientId = resolveConnectCliOAuthClientId();
  if (clerkPublishableKey && clientId) {
    return buildConnectClerkAuthorizeUrl({
      authorizationEndpoint: `${clerkFrontendApiUrlFromPublishableKey(clerkPublishableKey)}/oauth/authorize`,
      clientId,
      redirectUri: connectLoopbackRedirectUri(request.loopbackPort),
      scopes: CONNECT_OAUTH_SCOPES,
      state: request.state,
      challenge: request.challenge,
    });
  }
  if (!iamIssuer || !iamClientId) {
    return null;
  }
  return buildConnectClerkAuthorizeUrl({
    authorizationEndpoint: `${iamIssuer}/oauth/authorize`,
    clientId: iamClientId,
    redirectUri: connectLoopbackRedirectUri(request.loopbackPort),
    scopes: CONNECT_OAUTH_SCOPES,
    state: request.state,
    challenge: request.challenge,
  });
}

/** Force a new document: fragment revisits do not reload, and Clerk suppresses post-sign-in session updates. */
export function connectCliSignInRedirectUrl(
  request: ConnectAuthorizeRequest,
  currentHref: string,
): string {
  return buildConnectCliClerkAuthorizeUrl(request) ?? currentHref;
}
