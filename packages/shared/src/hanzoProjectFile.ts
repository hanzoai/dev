import * as Exit from "effect/Exit";
import * as Schema from "effect/Schema";

import { HanzoProjectFile, HANZO_PROJECT_FILE_SCHEMA_URL } from "@hanzodev/contracts";

import { fromLenientJson } from "./schemaJson.ts";

export const HanzoProjectFileFromJson = fromLenientJson(HanzoProjectFile);

const decodeHanzoProjectFile = Schema.decodeExit(HanzoProjectFileFromJson);

export function parseHanzoProjectFile(contents: string): HanzoProjectFile | null {
  const decoded = decodeHanzoProjectFile(contents);
  return Exit.isSuccess(decoded) ? decoded.value : null;
}

export function buildHanzoProjectFileJsonSchema(): Record<string, unknown> {
  const document = Schema.toJsonSchemaDocument(HanzoProjectFile, { onExcessProperty: "error" });
  const jsonSchema: Record<string, unknown> = {
    $schema: "https://json-schema.org/draft/2020-12/schema",
    $id: HANZO_PROJECT_FILE_SCHEMA_URL,
    ...document.schema,
  };
  if (document.definitions && Object.keys(document.definitions).length > 0) {
    jsonSchema.$defs = document.definitions;
  }
  return jsonSchema;
}
