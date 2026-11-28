/** Copy text; true once it is on the clipboard, false where the browser has none or refuses it (nothing is thrown). */
export const copy = async (text: string): Promise<boolean> => {
  try {
    await navigator.clipboard.writeText(text)
    return true
  } catch {
    return false
  }
}
