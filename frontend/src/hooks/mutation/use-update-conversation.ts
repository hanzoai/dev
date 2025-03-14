import { useQueryClient, useMutation } from "@tanstack/react-query";
import Hanzo from "#/api/hanzo";
import { Conversation } from "#/api/hanzo.types";

export const useUpdateConversation = () => {
  const queryClient = useQueryClient();

  return useMutation({
    mutationFn: (variables: {
      id: string;
      conversation: Partial<Omit<Conversation, "id">>;
    }) =>
      Hanzo.updateUserConversation(variables.id, variables.conversation),
    onSuccess: () => {
      queryClient.invalidateQueries({ queryKey: ["user", "conversations"] });
    },
  });
};
