import { markEmbedded } from "./context";

// Whatever a host draws from the core (DevProvider, DevHome), the core is embedded: it has no
// primary environment of its own to discover. Imported for this effect, before any service starts.
markEmbedded();
