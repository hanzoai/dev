export interface SignInProps {
  forceRedirectUrl?: string;
  signUpForceRedirectUrl?: string;
}

export function resolveSignInProps(href: string, isElectron: boolean): SignInProps {
  if (isElectron) {
    // Electron routes through the hash, so reset any sign-in virtual pathname without losing the Hanzo page.
    const redirectUrl = new URL(href);
    redirectUrl.pathname = "/";
    redirectUrl.search = "";

    return {
      forceRedirectUrl: redirectUrl.toString(),
      signUpForceRedirectUrl: redirectUrl.toString(),
    };
  }
  // Set sign-up's redirect too; switching from sign-in otherwise falls back to the modal's opening URL.
  return { forceRedirectUrl: href, signUpForceRedirectUrl: href };
}
