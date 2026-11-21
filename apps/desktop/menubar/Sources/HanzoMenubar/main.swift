import AppKit
import Combine
import SwiftUI
import UserNotifications

/// The one menu-bar item for Hanzo Dev. The Dev app launches this helper with `HANZO_DEV_URL` and
/// `HANZO_DEV_TOKEN`, reads one action per line from its stdout, and closes its stdin to stop it.
@MainActor
final class Controller: NSObject, NSApplicationDelegate {
  private let store = Store(client: Endpoint.fromEnvironment().map(Client.init))
  private var item: NSStatusItem!
  private var panel: NSPanel!
  private var monitor: Any?
  private var sizing: AnyCancellable?

  func applicationDidFinishLaunching(_ note: Notification) {
    for name in ["Zen-Regular", "Zen-Bold"] {
      if let url = Bundle.main.url(forResource: name, withExtension: "ttf") {
        CTFontManagerRegisterFontsForURL(url as CFURL, .process, nil)
      }
    }
    UNUserNotificationCenter.current().requestAuthorization(options: [.alert, .sound]) { _, _ in }

    item = NSStatusBar.system.statusItem(withLength: NSStatusItem.variableLength)
    item.button?.image = devGlyph()
    item.button?.imagePosition = .imageLeft
    item.button?.target = self
    item.button?.action = #selector(toggle)
    store.onBarChange = { [weak self] in self?.drawBar() }

    let host = NSHostingView(rootView: Panel(store: store) { [weak self] in self?.act($0) })
    panel = NSPanel(
      contentRect: .zero, styleMask: [.borderless, .nonactivatingPanel], backing: .buffered, defer: true)
    panel.contentView = host
    panel.isOpaque = false
    panel.backgroundColor = .clear
    panel.hasShadow = true
    panel.level = .popUpMenu
    panel.appearance = NSAppearance(named: .darkAqua)
    host.wantsLayer = true
    host.layer?.cornerRadius = 12
    host.layer?.masksToBounds = true
    host.layer?.borderWidth = 0.5
    host.layer?.borderColor = NSColor.white.withAlphaComponent(0.14).cgColor
    sizing = store.objectWillChange.receive(on: RunLoop.main).sink { [weak self] _ in self?.fit() }

    store.start()
    Task.detached {
      // The parent closing stdin is the only stop signal that survives it crashing.
      while FileHandle.standardInput.availableData.count > 0 {}
      exit(0)
    }
  }

  private func drawBar() {
    let title = store.barTitle
    item.button?.attributedTitle = NSAttributedString(
      string: title.isEmpty ? "" : " " + title,
      attributes: [.font: NSFont.monospacedDigitSystemFont(ofSize: 12, weight: .medium)])
    item.length = NSStatusItem.variableLength
  }

  private func fit() {
    DispatchQueue.main.async { [self] in
      guard let host = panel.contentView else { return }
      let size = host.fittingSize
      var frame = panel.frame
      frame.origin.y += frame.height - size.height
      frame.size = size
      panel.setFrame(frame, display: true)
    }
  }

  @objc private func toggle() { panel.isVisible ? close() : open() }

  private func open() {
    guard let button = item.button, let window = button.window, let host = panel.contentView else { return }
    let size = host.fittingSize
    let anchor = window.convertToScreen(button.convert(button.bounds, to: nil))
    let screen = window.screen?.visibleFrame ?? .zero
    let x = min(max(anchor.midX - size.width / 2, screen.minX + 8), screen.maxX - size.width - 8)
    panel.setFrame(NSRect(x: x, y: anchor.minY - size.height - 6, width: size.width, height: size.height), display: true)
    panel.orderFrontRegardless()
    store.setVisible(true)
    monitor = NSEvent.addGlobalMonitorForEvents(matching: [.leftMouseDown, .rightMouseDown]) { [weak self] _ in
      Task { @MainActor in self?.close() }
    }
  }

  private func close() {
    panel.orderOut(nil)
    store.setVisible(false)
    if let monitor { NSEvent.removeMonitor(monitor) }
    monitor = nil
  }

  private func act(_ action: String) {
    close()
    print(action)
    fflush(stdout)
    if action == "quit" { NSApp.terminate(nil) }
  }
}

let app = NSApplication.shared
app.setActivationPolicy(.accessory)
let controller = Controller()
app.delegate = controller
app.run()
