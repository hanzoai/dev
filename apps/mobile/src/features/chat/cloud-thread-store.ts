import type { StoredThread, ThreadStore } from "@hanzodev/client-runtime/cloud-chat";
import { MAX_STORED_THREADS } from "@hanzodev/client-runtime/cloud-chat";
import type { SQLiteDatabase } from "expo-sqlite";

let opening: Promise<SQLiteDatabase> | null = null;

function open(): Promise<SQLiteDatabase> {
  opening ??= (async () => {
    const SQLite = await import("expo-sqlite");
    const database = await SQLite.openDatabaseAsync("hanzo-cloud-chat.db");
    await database.execAsync(`
      CREATE TABLE IF NOT EXISTS cloud_threads (
        owner TEXT NOT NULL,
        key TEXT NOT NULL,
        payload TEXT NOT NULL,
        updated_at INTEGER NOT NULL,
        PRIMARY KEY (owner, key)
      ) WITHOUT ROWID;
    `);
    return database;
  })().catch((cause: unknown) => {
    opening = null;
    throw cause;
  });
  return opening;
}

/** The device copy of cloud chat threads, one SQLite row per thread and account. */
export const cloudThreadStore: ThreadStore = {
  async load(owner) {
    const database = await open();
    const rows = await database.getAllAsync<{ readonly payload: string }>(
      "SELECT payload FROM cloud_threads WHERE owner = ? ORDER BY updated_at DESC LIMIT ?",
      owner,
      MAX_STORED_THREADS,
    );
    const threads: StoredThread[] = [];
    for (const row of rows) {
      try {
        threads.push(JSON.parse(row.payload) as StoredThread);
      } catch {
        // A damaged row is skipped; the rest of the list still loads.
      }
    }
    return threads;
  },
  async save(owner, thread) {
    const database = await open();
    await database.runAsync(
      "INSERT OR REPLACE INTO cloud_threads (owner, key, payload, updated_at) VALUES (?, ?, ?, ?)",
      owner,
      thread.key,
      JSON.stringify(thread),
      thread.updatedAt,
    );
    await database.runAsync(
      "DELETE FROM cloud_threads WHERE owner = ? AND key NOT IN (SELECT key FROM cloud_threads WHERE owner = ? ORDER BY updated_at DESC LIMIT ?)",
      owner,
      owner,
      MAX_STORED_THREADS,
    );
  },
};
