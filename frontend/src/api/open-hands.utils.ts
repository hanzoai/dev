import { ErrorResponse, FileUploadSuccessResponse } from "./open-hands.types";

export const isHanzoErrorResponse = (
  data: ErrorResponse | FileUploadSuccessResponse,
): data is ErrorResponse =>
  typeof data === "object" && data !== null && "error" in data;
