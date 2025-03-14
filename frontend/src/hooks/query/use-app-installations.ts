import { useQuery } from "@tanstack/react-query";
import { useConfig } from "./use-config";
import Hanzo from "#/api/hanzo";
import { useAuth } from "#/context/auth-context";

export const useAppInstallations = () => {
  const { data: config } = useConfig();
  const { githubTokenIsSet } = useAuth();

  return useQuery({
    queryKey: ["installations", githubTokenIsSet, config?.GITHUB_CLIENT_ID],
    queryFn: Hanzo.getGitHubUserInstallationIds,
    enabled:
      githubTokenIsSet &&
      !!config?.GITHUB_CLIENT_ID &&
      config?.APP_MODE === "saas",
    staleTime: 1000 * 60 * 5, // 5 minutes
    gcTime: 1000 * 60 * 15, // 15 minutes
  });
};
