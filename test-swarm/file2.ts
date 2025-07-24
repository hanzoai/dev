// TypeScript utility functions
export function formatDate(date: Date): string {
  return date.toISOString().split('T')[0];
}

export function parseJSON<T>(json: string): T | null {
  try {
    return JSON.parse(json);
  } catch {
    return null;
  }
}