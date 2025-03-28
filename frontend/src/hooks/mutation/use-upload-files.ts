import { useMutation } from "@tanstack/react-query";
import Dev from "#/api/open-hands";
import { useConversation } from "#/context/conversation-context";

type UploadFilesArgs = {
  files: File[];
};

export const useUploadFiles = () => {
  const { conversationId } = useConversation();

  return useMutation({
    mutationFn: ({ files }: UploadFilesArgs) =>
      Dev.uploadFiles(conversationId, files),
  });
};
