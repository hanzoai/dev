import { useQuery } from "@tanstack/react-query";
import React from "react";
import Dev from "#/api/dev";
import { useConfig } from "./use-config";
import { useAuth } from "#/context/auth-context";

export const useIsAuthed = () => {
  const { githubTokenIsSet } = useAuth();
  const { data: config } = useConfig();

  const appMode = React.useMemo(() => config?.APP_MODE, [config]);

  return useQuery({
    queryKey: ["user", "authenticated", githubTokenIsSet, appMode],
    queryFn: () => Dev.authenticate(appMode!),
    enabled: !!appMode,
    staleTime: 1000 * 60 * 5, // 5 minutes
    gcTime: 1000 * 60 * 15, // 15 minutes
    retry: false,
    meta: {
      disableToast: true,
    },
  });
};
