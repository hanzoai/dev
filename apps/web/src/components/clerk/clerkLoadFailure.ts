import { useClerk } from "../../cloud/hanzoWebAuth";

export function useClerkLoadFailed() {
  return useClerk().status === "error";
}

export function retryClerkLoad() {
  // Clerk caches its failed script load for the life of the page and never
  // retries, so a reload is the only recovery.
  window.location.reload();
}
