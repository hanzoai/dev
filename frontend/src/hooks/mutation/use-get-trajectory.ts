import { useMutation } from "@tanstack/react-query";
import Hanzo from "#/api/open-hands";

export const useGetTrajectory = () =>
  useMutation({
    mutationFn: (cid: string) => Hanzo.getTrajectory(cid),
  });
