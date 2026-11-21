import { useGT } from "gt-react";
import { useEffect, useRef, useState } from "react";

import { Button } from "../ui/button";
import { Dialog, DialogFooter, DialogHeader, DialogPopup, DialogTitle } from "../ui/dialog";
import { composerFloatingLayerProps } from "./composerEventScope";

const WIDTH = 960;
const HEIGHT = 600;

/** A drawing pad. "Attach" hands the drawing over as a PNG, on white so it reads in any theme. */
function paintWhite(canvas: HTMLCanvasElement | null) {
  const context = canvas?.getContext("2d");
  if (!context) return;
  context.fillStyle = "#ffffff";
  context.fillRect(0, 0, WIDTH, HEIGHT);
}

export function ComposerSketchDialog({
  open,
  onClose,
  onAttach,
}: {
  readonly open: boolean;
  readonly onClose: () => void;
  readonly onAttach: (file: File) => void;
}) {
  const gt = useGT();
  const canvasRef = useRef<HTMLCanvasElement>(null);
  const drawing = useRef(false);
  const [empty, setEmpty] = useState(true);

  const clear = () => {
    paintWhite(canvasRef.current);
    setEmpty(true);
  };
  // oxlint-disable-next-line hanzo/no-unreasoned-state-sync -- paints the white page when the pad opens
  useEffect(() => {
    if (open) requestAnimationFrame(() => paintWhite(canvasRef.current));
  }, [open]);

  const point = (event: React.PointerEvent<HTMLCanvasElement>) => {
    const rect = event.currentTarget.getBoundingClientRect();
    return {
      x: ((event.clientX - rect.left) / rect.width) * WIDTH,
      y: ((event.clientY - rect.top) / rect.height) * HEIGHT,
    };
  };
  const stroke = (event: React.PointerEvent<HTMLCanvasElement>, begin: boolean) => {
    const context = canvasRef.current?.getContext("2d");
    if (!context) return;
    const { x, y } = point(event);
    context.lineWidth = 3;
    context.lineCap = "round";
    context.strokeStyle = "#111111";
    if (begin) {
      context.beginPath();
      context.moveTo(x, y);
    }
    context.lineTo(x, y);
    context.stroke();
    setEmpty(false);
  };

  const attach = () => {
    canvasRef.current?.toBlob((blob) => {
      if (!blob) return;
      onAttach(new File([blob], `sketch-${Date.now()}.png`, { type: "image/png" }));
      onClose();
    }, "image/png");
  };

  return (
    <Dialog open={open} onOpenChange={(next) => !next && onClose()}>
      <DialogPopup
        {...composerFloatingLayerProps}
        className="max-w-3xl"
        data-testid="composer-sketch"
      >
        <DialogHeader>
          <DialogTitle>{gt("Sketch")}</DialogTitle>
        </DialogHeader>
        <canvas
          ref={canvasRef}
          width={WIDTH}
          height={HEIGHT}
          aria-label={gt("Drawing pad")}
          className="mx-6 w-[calc(100%-3rem)] touch-none rounded-lg border border-border bg-white"
          onPointerDown={(event) => {
            event.currentTarget.setPointerCapture(event.pointerId);
            drawing.current = true;
            stroke(event, true);
          }}
          onPointerMove={(event) => {
            if (drawing.current) stroke(event, false);
          }}
          onPointerUp={() => {
            drawing.current = false;
          }}
        />
        <DialogFooter>
          <Button variant="outline" onClick={clear} disabled={empty}>
            {gt("Clear")}
          </Button>
          <Button onClick={attach} disabled={empty}>
            {gt("Attach")}
          </Button>
        </DialogFooter>
      </DialogPopup>
    </Dialog>
  );
}
