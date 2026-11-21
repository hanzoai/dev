import { describe, expect, it } from "vite-plus/test";

import { KEYMAP_ROWS, resolveCommand, setKeymapOverrides, shortcutFor } from "./keymap";

const press = (init: {
  key: string;
  code?: string;
  metaKey?: boolean;
  shiftKey?: boolean;
  ctrlKey?: boolean;
  altKey?: boolean;
}) => ({
  key: init.key,
  code: init.code ?? "",
  metaKey: init.metaKey ?? false,
  ctrlKey: init.ctrlKey ?? false,
  shiftKey: init.shiftKey ?? false,
  altKey: init.altKey ?? false,
});

describe("the one keymap", () => {
  it("resolves the window commands a host needs", () => {
    expect(resolveCommand(press({ key: "k", metaKey: true }))).toBe("commandPalette.toggle");
    expect(resolveCommand(press({ key: "n", metaKey: true }))).toBe("chat.new");
    expect(resolveCommand(press({ key: "b", metaKey: true }))).toBe("sidebar.toggle");
    expect(resolveCommand(press({ key: "3", metaKey: true }), { hostDesktop: true })).toBe(
      "section.3",
    );
    expect(resolveCommand(press({ key: "3", metaKey: true }))).toBeNull(); // the browser keeps it
    expect(resolveCommand(press({ key: "3", code: "Digit3", ctrlKey: true, altKey: true }))).toBe(
      "section.3",
    );
    expect(resolveCommand(press({ key: "[", metaKey: true }))).toBe("navigation.back");
    expect(resolveCommand(press({ key: "]", metaKey: true }))).toBe("navigation.forward");
    expect(resolveCommand(press({ key: ",", metaKey: true }))).toBe("settings.open");
    expect(resolveCommand(press({ key: "/", metaKey: true }))).toBe("keybindings.open");
    expect(
      resolveCommand(press({ key: "?", code: "Slash", shiftKey: true }), { editableFocus: false }),
    ).toBe("keybindings.open");
    expect(resolveCommand(press({ key: "l", metaKey: true }))).toBe("composer.focus");
    expect(resolveCommand(press({ key: "0", metaKey: true }))).toBe("sidebar.focus");
    expect(resolveCommand(press({ key: "F6" }))).toBe("workspace.focusNext");
  });

  it("does not take a plain ? while typing", () => {
    expect(
      resolveCommand(press({ key: "?", code: "Slash", shiftKey: true }), { editableFocus: true }),
    ).toBeNull();
  });

  it("rebinds a command and restores it", () => {
    setKeymapOverrides([{ key: "mod+shift+p", command: "commandPalette.toggle" }]);
    expect(resolveCommand(press({ key: "p", metaKey: true, shiftKey: true }))).toBe(
      "commandPalette.toggle",
    );
    expect(resolveCommand(press({ key: "k", metaKey: true }))).not.toBe("commandPalette.toggle");
    setKeymapOverrides([]);
    expect(resolveCommand(press({ key: "k", metaKey: true }))).toBe("commandPalette.toggle");
  });

  it("every row the overlay lists has a shortcut or is rebindable", () => {
    const unbound = KEYMAP_ROWS.filter((row) => shortcutFor(row.command) === null).map(
      (row) => row.command,
    );
    expect(unbound).toEqual([]);
  });
});
