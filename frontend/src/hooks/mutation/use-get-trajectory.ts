import { useMutation } from "@tanstack/react-query";
import Hanzo from "#/api/hanzo";

export const useGetTrajectory = () =>
  useMutation({
    mutationFn: (cid: string) => Hanzo.getTrajectory(cid),
  });
