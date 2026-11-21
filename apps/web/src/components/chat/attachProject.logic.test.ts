import { describe, expect, it } from "vite-plus/test";

import { attachableProjects } from "./attachProject.logic";

const project = (id: string, extra: Record<string, unknown> = {}) =>
  ({
    id,
    environmentId: "env-1",
    title: id,
    workspaceRoot: `/work/${id}`,
    ...extra,
  }) as never;

describe("attachableProjects", () => {
  it("offers the environment's projects, never the scratch project, the current one or a deleted one", () => {
    const projects = [
      project("scratch", { workspaceRoot: "/scratch/a", isScratch: true }),
      project("app"),
      project("api"),
      project("gone", { deletedAt: "2026-01-01" }),
      { ...project("other"), environmentId: "env-2" },
    ];
    const names = attachableProjects(projects as never, {
      environmentId: "env-1" as never,
      currentProjectId: "api" as never,
      scratchWorkspaceRoot: "/scratch",
    }).map((p) => p.id);
    expect(names).toEqual(["app"]);
  });
});
