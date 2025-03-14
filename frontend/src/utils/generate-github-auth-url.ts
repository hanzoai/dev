/**
 * Generates a URL to redirect to for GitHub OAuth
 * @param clientId The GitHub OAuth client ID
 * @param requestUrl The URL of the request
 * @param offline True for offline session, defaults to false
 * @returns The URL to redirect to for GitHub OAuth
 */
export const generateGitHubAuthUrl = (clientId: string, requestUrl: URL) => {
  const redirectUri = `${requestUrl.origin}/oauth/keycloak/callback`;
  const authUrl = requestUrl.hostname
    .replace(/(^|\.)staging\.hanzo\.build$/, "$1auth.staging.hanzo.build")
    .replace(/(^|\.)app\.hanzo\.build$/, "auth.app.hanzo.build")
    .replace(/(^|\.)localhost$/, "auth.staging.hanzo.build");
  const scope = "openid email profile";
  return `https://${authUrl}/realms/hanzo/protocol/openid-connect/auth?client_id=github&response_type=code&redirect_uri=${encodeURIComponent(redirectUri)}&scope=${encodeURIComponent(scope)}`;
};
