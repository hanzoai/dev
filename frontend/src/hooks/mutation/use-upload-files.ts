import { useMutation } from "@tanstack/react-query";
import Hanzo from "#/api/hanzo";
import { useConversation } from "#/context/conversation-context";

type UploadFilesArgs = {
  files: File[];
};

export const useUploadFiles = () => {
  const { conversationId } = useConversation();

  return useMutation({
    mutationFn: ({ files }: UploadFilesArgs) =>
      Hanzo.uploadFiles(conversationId, files),
  });
};
