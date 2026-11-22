// Hermes (React Native) does not ship ES2023 non-mutating Array.prototype methods
// (`toSorted`, `toReversed`, `toSpliced`, `with`), which throws `TypeError: undefined is not a function`
// whenever shared client-runtime or UI modules call them.
const arrayProto = Array.prototype as unknown as Record<string, unknown>;

if (typeof arrayProto.toSorted !== "function") {
  Object.defineProperty(Array.prototype, "toSorted", {
    configurable: true,
    writable: true,
    value<T>(this: readonly T[], compareFn?: (a: T, b: T) => number): T[] {
      return [...this].sort(compareFn);
    },
  });
}

if (typeof arrayProto.toReversed !== "function") {
  Object.defineProperty(Array.prototype, "toReversed", {
    configurable: true,
    writable: true,
    value<T>(this: readonly T[]): T[] {
      return [...this].reverse();
    },
  });
}

if (typeof arrayProto.toSpliced !== "function") {
  Object.defineProperty(Array.prototype, "toSpliced", {
    configurable: true,
    writable: true,
    value<T>(this: readonly T[], start: number, deleteCount?: number, ...items: T[]): T[] {
      const copy = [...this];
      if (arguments.length <= 1) {
        copy.splice(start);
      } else {
        copy.splice(start, deleteCount ?? 0, ...items);
      }
      return copy;
    },
  });
}

if (typeof arrayProto.with !== "function") {
  Object.defineProperty(Array.prototype, "with", {
    configurable: true,
    writable: true,
    value<T>(this: readonly T[], index: number, value: T): T[] {
      const copy = [...this];
      const len = copy.length;
      const actual = index < 0 ? len + index : index;
      if (actual < 0 || actual >= len) {
        throw new RangeError(`Invalid index: ${index}`);
      }
      copy[actual] = value;
      return copy;
    },
  });
}
