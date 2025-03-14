import { HanzoAction } from "./actions";
import { HanzoObservation } from "./observations";
import { HanzoVariance } from "./variances";

export type HanzoParsedEvent =
  | HanzoAction
  | HanzoObservation
  | HanzoVariance;
