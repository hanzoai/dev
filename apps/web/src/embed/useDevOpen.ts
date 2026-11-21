import { useEmbed } from "./context";
import { openRepository, type OpenRepository } from "./openRepository";

/**
 * For a host's deep link: opens a repository in the runtime as a project with a new thread on
 * it, and answers the thread's address for the host to go to. Call under `DevProvider`.
 */
export function useDevOpen(): (input: OpenRepository) => Promise<string> {
  const embed = useEmbed();
  return (input) => {
    if (!embed) throw new Error("useDevOpen needs DevProvider above it.");
    return openRepository(embed, input);
  };
}
