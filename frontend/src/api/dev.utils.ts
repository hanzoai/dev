import { ErrorResponse, FileUploadSuccessResponse } from "./dev.types";

export const isDevErrorResponse = (
  data: ErrorResponse | FileUploadSuccessResponse,
): data is ErrorResponse =>
  typeof data === "object" && data !== null && "error" in data;
