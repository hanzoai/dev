import { useMutation } from "@tanstack/react-query";
import Hanzo from "#/api/hanzo";

export const useCreateStripeCheckoutSession = () =>
  useMutation({
    mutationFn: async (variables: { amount: number }) => {
      const redirectUrl = await Hanzo.createCheckoutSession(
        variables.amount,
      );
      window.location.href = redirectUrl;
    },
  });
