import { useQuery } from "@tanstack/react-query";
import { useConfig } from "./use-config";
import Hanzo from "#/api/hanzo";
import { BILLING_SETTINGS } from "#/utils/feature-flags";

export const useBalance = () => {
  const { data: config } = useConfig();

  return useQuery({
    queryKey: ["user", "balance"],
    queryFn: Hanzo.getBalance,
    enabled: config?.APP_MODE === "saas" && BILLING_SETTINGS(),
  });
};
