import { useHanzoSession } from "../../cloud/hanzoWebAuth";

import { isElectron } from "../../env";
import { resolveSignInProps } from "./authRedirect";

export function useHanzoConnectAuthPrompt() {
  const session = useHanzoSession();
  const openAuthPrompt = () => {
    session.openSignIn(resolveSignInProps(window.location.href, isElectron));
  };
  return { openAuthPrompt };
}
