import { useQuery } from "@tanstack/react-query";
import Dev from "#/api/dev";
import { useIsAuthed } from "./use-is-authed";

export const useUserConversations = () => {
  const { data: userIsAuthenticated } = useIsAuthed();

  return useQuery({
    queryKey: ["user", "conversations"],
    queryFn: Dev.getUserConversations,
    enabled: !!userIsAuthenticated,
  });
};
