import { useMutation } from "@tanstack/react-query";
import Dev from "#/api/dev";

export const useCreateStripeCheckoutSession = () =>
  useMutation({
    mutationFn: async (variables: { amount: number }) => {
      const redirectUrl = await Dev.createCheckoutSession(
        variables.amount,
      );
      window.location.href = redirectUrl;
    },
  });
