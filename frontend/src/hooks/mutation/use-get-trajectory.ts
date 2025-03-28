import { useMutation } from "@tanstack/react-query";
import Dev from "#/api/open-hands";

export const useGetTrajectory = () =>
  useMutation({
    mutationFn: (cid: string) => Dev.getTrajectory(cid),
  });
