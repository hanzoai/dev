import { useQuery } from "@tanstack/react-query";
import Hanzo from "#/api/hanzo";

export const useConfig = () =>
  useQuery({
    queryKey: ["config"],
    queryFn: Hanzo.getConfig,
    staleTime: 1000 * 60 * 5, // 5 minutes
    gcTime: 1000 * 60 * 15, // 15 minutes
  });
