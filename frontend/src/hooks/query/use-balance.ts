import { useQuery } from "@tanstack/react-query";
import { useConfig } from "./use-config";
import Dev from "#/api/open-hands";

export const useBalance = () => {
  const { data: config } = useConfig();

  return useQuery({
    queryKey: ["user", "balance"],
    queryFn: Dev.getBalance,
    enabled:
      config?.APP_MODE === "saas" && config?.FEATURE_FLAGS.ENABLE_BILLING,
  });
};
