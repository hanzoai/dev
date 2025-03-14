import { useMutation } from "@tanstack/react-query";
import { Feedback } from "#/api/hanzo.types";
import Hanzo from "#/api/hanzo";
import { useConversation } from "#/context/conversation-context";
import { displayErrorToast } from "#/utils/custom-toast-handlers";

type SubmitFeedbackArgs = {
  feedback: Feedback;
};

export const useSubmitFeedback = () => {
  const { conversationId } = useConversation();
  return useMutation({
    mutationFn: ({ feedback }: SubmitFeedbackArgs) =>
      Hanzo.submitFeedback(conversationId, feedback),
    onError: (error) => {
      displayErrorToast(error.message);
    },
  });
};
