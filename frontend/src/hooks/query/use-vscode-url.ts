import { useQuery } from "@tanstack/react-query";
import Dev from "#/api/dev";
import { useConversation } from "#/context/conversation-context";

export const useVSCodeUrl = (config: { enabled: boolean }) => {
  const { conversationId } = useConversation();

  const data = useQuery({
    queryKey: ["vscode_url", conversationId],
    queryFn: () => {
      if (!conversationId) throw new Error("No conversation ID");
      return Dev.getVSCodeUrl(conversationId);
    },
    enabled: !!conversationId && config.enabled,
    refetchOnMount: true,
  });

  return data;
};
