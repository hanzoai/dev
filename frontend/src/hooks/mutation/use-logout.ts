import { useMutation, useQueryClient } from "@tanstack/react-query";
import Hanzo from "#/api/hanzo";
import { useAuth } from "#/context/auth-context";

export const useLogout = () => {
  const { setGitHubTokenIsSet } = useAuth();
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: Hanzo.logout,
    onSuccess: async () => {
      setGitHubTokenIsSet(false);
      await queryClient.invalidateQueries();
    },
  });
};
