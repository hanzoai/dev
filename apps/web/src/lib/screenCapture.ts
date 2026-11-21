/** Whether this browser can capture the screen. */
export function canCaptureScreen(): boolean {
  return (
    typeof navigator !== "undefined" &&
    typeof navigator.mediaDevices?.getDisplayMedia === "function"
  );
}

/**
 * The screen (or window or tab) the person picks, as one PNG. Resolves null when they dismiss the
 * picker, so a cancelled capture is not an error.
 */
export async function captureScreenshot(): Promise<File | null> {
  let stream: MediaStream;
  try {
    stream = await navigator.mediaDevices.getDisplayMedia({ video: true, audio: false });
  } catch (cause) {
    if (
      cause instanceof DOMException &&
      (cause.name === "NotAllowedError" || cause.name === "AbortError")
    ) {
      return null;
    }
    throw cause;
  }
  try {
    const video = document.createElement("video");
    video.muted = true;
    video.srcObject = stream;
    await video.play();
    const canvas = document.createElement("canvas");
    canvas.width = video.videoWidth;
    canvas.height = video.videoHeight;
    const context = canvas.getContext("2d");
    if (!context || canvas.width === 0) throw new Error("The screen could not be read.");
    context.drawImage(video, 0, 0);
    const blob = await new Promise<Blob | null>((resolve) => canvas.toBlob(resolve, "image/png"));
    if (!blob) throw new Error("The screenshot could not be saved.");
    return new File([blob], `screenshot-${new Date().toISOString().replaceAll(":", "-")}.png`, {
      type: "image/png",
    });
  } finally {
    for (const track of stream.getTracks()) track.stop();
  }
}
