import { useQuery } from "@tanstack/react-query";
import Dev from "#/api/dev";

export const useConfig = () =>
  useQuery({
    queryKey: ["config"],
    queryFn: Dev.getConfig,
    staleTime: 1000 * 60 * 5, // 5 minutes
    gcTime: 1000 * 60 * 15, // 15 minutes
  });
