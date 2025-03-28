import { useQuery } from "@tanstack/react-query";
import Dev from "#/api/dev";
import { useConversation } from "#/context/conversation-context";

interface UseListFileConfig {
  path: string;
}

export const useListFile = (config: UseListFileConfig) => {
  const { conversationId } = useConversation();
  return useQuery({
    queryKey: ["file", conversationId, config.path],
    queryFn: () => Dev.getFile(conversationId, config.path),
    enabled: false, // don't fetch by default, trigger manually via `refetch`
  });
};
