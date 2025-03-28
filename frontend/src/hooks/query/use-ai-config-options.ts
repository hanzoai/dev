import { useQuery } from "@tanstack/react-query";
import Dev from "#/api/dev";

const fetchAiConfigOptions = async () => ({
  models: await Dev.getModels(),
  agents: await Dev.getAgents(),
  securityAnalyzers: await Dev.getSecurityAnalyzers(),
});

export const useAIConfigOptions = () =>
  useQuery({
    queryKey: ["ai-config-options"],
    queryFn: fetchAiConfigOptions,
    staleTime: 1000 * 60 * 5, // 5 minutes
    gcTime: 1000 * 60 * 15, // 15 minutes
  });
