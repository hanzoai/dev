// `decode-named-character-reference/index.dom.js` calls `document.createElement("i")` at module
// load time, which throws `ReferenceError: document is not defined` if a host or test runner
// imports `@hanzo/dev` in Node. Defer the element creation to first call so the embed bundle
// imports cleanly in both browser and Node environments.
let element: HTMLElement | undefined;

export function decodeNamedCharacterReference(value: string): string | false {
  if (typeof document === "undefined") return false;
  element ??= document.createElement("i");
  const entity = `&${value};`;
  element.innerHTML = entity;
  const decoded = element.textContent;
  if (decoded.charCodeAt(decoded.length - 1) === 59 && value !== "semi") return false;
  return decoded === entity ? false : decoded;
}
