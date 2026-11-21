import Foundation
import UserNotifications

/// What the panel and the status item draw. The server owns readings, thresholds and settings.
@MainActor
final class Store: ObservableObject {
  @Published private(set) var snapshots: [Snapshot] = []
  @Published private(set) var updated: Date?
  @Published private(set) var failure: String?
  @Published private(set) var showInBar = false
  @Published private(set) var barTitle = ""
  @Published private(set) var refreshing = false
  @Published var selected: String?

  var onBarChange: (() -> Void)?
  private let client: Client?
  private var loop: Task<Void, Never>?
  private var fast = false

  init(client: Client?) { self.client = client }

  /// Hanzo's own balance and plan first, then by name.
  private static func order(_ s: [Snapshot]) -> [Snapshot] {
    s.sorted { ($0.isHanzo ? 0 : 1, $0.title) < ($1.isHanzo ? 0 : 1, $1.title) }
  }

  var current: Snapshot? { snapshots.first { $0.id == selected } ?? snapshots.first }

  func start() {
    loop = Task { [weak self] in
      while !Task.isCancelled {
        await self?.poll(force: false)
        try? await Task.sleep(for: .seconds(self?.fast == true ? 15 : 60))
      }
    }
  }

  func setVisible(_ open: Bool) {
    fast = open
    if open { Task { await poll(force: false) } }
  }

  func refresh() { Task { await poll(force: true) } }

  func setShowInBar(_ on: Bool) {
    guard let client else { return }
    showInBar = on
    Task {
      try? await client.setShowInMenuBar(on)
      await poll(force: false)
    }
  }

  private func poll(force: Bool) async {
    guard let client else {
      failure = "No Dev server"
      return
    }
    refreshing = true
    defer { refreshing = false }
    do {
      let summary = force ? try await client.refresh() : try await client.summary()
      let tray = try await client.tray()
      showInBar = try await client.showInMenuBar()
      snapshots = Store.order(summary.snapshots)
      updated = parseInstant(summary.generatedAt) ?? Date()
      failure = nil
      if tray.tray.title != barTitle {
        barTitle = tray.tray.title
        onBarChange?()
      }
      for alert in tray.alerts { Store.notify(alert) }
    } catch {
      failure = "Dev server unreachable"
    }
  }

  private static func notify(_ alert: Alert) {
    let content = UNMutableNotificationContent()
    content.title = alert.title
    content.body = alert.body
    content.sound = .default
    UNUserNotificationCenter.current().add(
      UNNotificationRequest(identifier: UUID().uuidString, content: content, trigger: nil))
  }
}
