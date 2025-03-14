import { useQuery } from "@tanstack/react-query";
import Hanzo from "#/api/hanzo";
import { useIsAuthed } from "./use-is-authed";

export const useUserConversations = () => {
  const { data: userIsAuthenticated } = useIsAuthed();

  return useQuery({
    queryKey: ["user", "conversations"],
    queryFn: Hanzo.getUserConversations,
    enabled: !!userIsAuthenticated,
  });
};
