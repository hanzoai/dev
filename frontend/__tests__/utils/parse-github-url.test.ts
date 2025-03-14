import { expect, test } from "vitest";
import { parseGithubUrl } from "../../src/utils/parse-github-url";

test("parseGithubUrl", () => {
  expect(
    parseGithubUrl("https://github.com/alexreardon/tiny-invariant"),
  ).toEqual(["alexreardon", "tiny-invariant"]);

  expect(parseGithubUrl("https://github.com/hanzoai/Hanzo")).toEqual([
    "hanzoai",
    "Hanzo",
  ]);

  expect(parseGithubUrl("https://github.com/hanzoai/")).toEqual([
    "hanzoai",
    "",
  ]);

  expect(parseGithubUrl("https://github.com/")).toEqual([]);
});
