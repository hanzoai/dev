import AppKit

/// The Dev mark `</>`: strokes only, centred on the canvas, a template image so macOS tints it
/// with the menu bar's own colour.
func devGlyph(side: CGFloat = 18, weight: CGFloat = 1.5) -> NSImage {
  let path = NSBezierPath()
  path.lineWidth = weight
  path.lineCapStyle = .round
  path.lineJoinStyle = .round
  func line(_ points: [(CGFloat, CGFloat)]) {
    path.move(to: NSPoint(x: points[0].0, y: points[0].1))
    for p in points.dropFirst() { path.line(to: NSPoint(x: p.0, y: p.1)) }
  }
  line([(5.25, 4.75), (1.75, 8.75), (5.25, 12.75)])
  line([(10.25, 3.75), (7.25, 13.75)])
  line([(12.25, 4.75), (15.75, 8.75), (12.25, 12.75)])
  let box = path.bounds
  let shift = AffineTransform(
    translationByX: side / 2 - box.midX, byY: side / 2 - box.midY)
  path.transform(using: shift)
  let image = NSImage(size: NSSize(width: side, height: side), flipped: true) { _ in
    NSColor.black.setStroke()
    path.stroke()
    return true
  }
  image.isTemplate = true
  return image
}
