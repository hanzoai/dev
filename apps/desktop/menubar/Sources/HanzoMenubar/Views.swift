import AppKit
import SwiftUI

// The tile, bar and switcher layouts are adapted from CodexBar (MIT); see NOTICE.

enum Look {
  static let background = Color(red: 0.039, green: 0.039, blue: 0.039)
  static let tile = Color.white.opacity(0.06)
  static let track = Color.white.opacity(0.14)
  static let line = Color.white.opacity(0.10)
  static let dim = Color.white.opacity(0.55)
  static let warn = Color(red: 0.96, green: 0.72, blue: 0.24)
  static let critical = Color(red: 0.99, green: 0.27, blue: 0.27)

  static func font(_ size: CGFloat, bold: Bool = false) -> Font {
    .custom(bold ? "Zen-Bold" : "Zen-Regular", size: size)
  }

  static func tint(_ percent: Double) -> Color {
    percent >= 90 ? critical : percent >= 75 ? warn : .white
  }
}

struct UsageBar: View {
  let percent: Double
  var height: CGFloat = 6

  var body: some View {
    Canvas { context, size in
      let radius = CGSize(width: size.height / 2, height: size.height / 2)
      let track = Path(roundedRect: CGRect(origin: .zero, size: size), cornerSize: radius)
      context.fill(track, with: .color(Look.track))
      let width = size.width * min(100, max(0, percent)) / 100
      guard width > 0 else { return }
      let fill = Path(roundedRect: CGRect(x: 0, y: 0, width: max(width, size.height), height: size.height), cornerSize: radius)
      context.fill(fill, with: .color(Look.tint(percent)))
    }
    .frame(height: height)
    .accessibilityLabel("\(Int(percent.rounded())) percent used")
  }
}

struct Chip: View {
  let snapshot: Snapshot
  let selected: Bool

  var body: some View {
    VStack(spacing: 4) {
      VStack(spacing: 1) {
        Text(providerName(snapshot.provider))
          .font(Look.font(11, bold: selected))
        if let account = snapshot.account, !account.isEmpty {
          Text(account).font(Look.font(9)).opacity(0.7)
        }
      }
      .lineLimit(1)
      UsageBar(percent: snapshot.primary?.usedPercent ?? 0, height: 3)
        .frame(width: 44)
        .opacity(snapshot.primary == nil ? 0.35 : 1)
    }
    .padding(.horizontal, 10)
    .padding(.vertical, 7)
    .background(selected ? Look.tile.opacity(2.2) : .clear, in: RoundedRectangle(cornerRadius: 8))
    .foregroundStyle(selected ? .white : Look.dim)
    .contentShape(Rectangle())
  }
}

struct Switcher: View {
  @ObservedObject var store: Store

  var body: some View {
    ScrollView(.horizontal, showsIndicators: false) {
      HStack(spacing: 2) {
        ForEach(store.snapshots) { s in
          Chip(snapshot: s, selected: s.id == store.current?.id)
            .onTapGesture { store.selected = s.id }
        }
      }
    }
  }
}

struct WindowRow: View {
  let window: UsageWindow

  var body: some View {
    VStack(alignment: .leading, spacing: 5) {
      Text(window.label).font(Look.font(13, bold: true))
      UsageBar(percent: window.usedPercent)
      HStack {
        Text("\(Int(window.usedPercent.rounded()))% used")
        Spacer()
        if let at = window.resetsAt.flatMap(parseInstant) {
          Text("Resets in \(countdown(to: at))").foregroundStyle(Look.dim)
        }
      }
      .font(Look.font(11))
    }
  }
}

struct Card: View {
  let snapshot: Snapshot

  var body: some View {
    VStack(alignment: .leading, spacing: 12) {
      HStack(alignment: .firstTextBaseline) {
        VStack(alignment: .leading, spacing: 2) {
          Text(snapshot.title).font(Look.font(15, bold: true))
          Text("Updated \(parseInstant(snapshot.fetchedAt).map { ago($0) } ?? "never")")
            .font(Look.font(11)).foregroundStyle(Look.dim)
        }
        Spacer()
        if let plan = snapshot.plan {
          Text(plan).font(Look.font(11, bold: true))
            .padding(.horizontal, 8).padding(.vertical, 3)
            .background(Look.tile, in: Capsule())
        }
      }
      if let credits = snapshot.credits, let balance = credits.balance {
        HStack {
          Text("Balance").font(Look.font(13, bold: true))
          Spacer()
          Text(money(balance, credits.currency)).font(Look.font(15, bold: true))
        }
      }
      ForEach(snapshot.windows, id: \.label) { WindowRow(window: $0) }
      if let status = snapshot.status, status.indicator != "none", status.indicator != "unknown" {
        HStack(spacing: 6) {
          Circle().fill(status.indicator == "minor" ? Look.warn : Look.critical).frame(width: 7, height: 7)
          Text(status.description ?? status.indicator).font(Look.font(11))
        }
      }
      if let error = snapshot.error {
        Text(error).font(Look.font(11)).foregroundStyle(Look.dim)
      }
      if snapshot.windows.isEmpty, snapshot.credits?.balance == nil, snapshot.error == nil {
        Text("No usage yet").font(Look.font(11)).foregroundStyle(Look.dim)
      }
    }
    .frame(maxWidth: .infinity, alignment: .leading)
  }
}

struct Row: View {
  let symbol: String
  let title: String
  var trailing: String?
  let action: () -> Void
  @State private var hover = false

  var body: some View {
    Button(action: action) {
      HStack(spacing: 10) {
        Image(systemName: symbol).frame(width: 16).foregroundStyle(Look.dim)
        Text(title).font(Look.font(13))
        Spacer()
        if let trailing { Text(trailing).font(Look.font(11)).foregroundStyle(Look.dim) }
      }
      .padding(.horizontal, 8).padding(.vertical, 6)
      .background(hover ? Look.tile : .clear, in: RoundedRectangle(cornerRadius: 6))
      .contentShape(Rectangle())
    }
    .buttonStyle(.plain)
    .onHover { hover = $0 }
  }
}

struct Panel: View {
  @ObservedObject var store: Store
  let act: (String) -> Void

  var body: some View {
    VStack(alignment: .leading, spacing: 10) {
      if store.snapshots.count > 1 { Switcher(store: store) }
      Divider().overlay(Look.line)
      if let snapshot = store.current {
        Card(snapshot: snapshot)
      } else {
        Text(store.failure ?? "No usage data yet").font(Look.font(13)).foregroundStyle(Look.dim)
      }
      Divider().overlay(Look.line)
      VStack(spacing: 0) {
        Row(symbol: "square.and.pencil", title: "New Thread", trailing: "⇧⌘I") { act("chat.new") }
        Row(symbol: "chevron.left.forwardslash.chevron.right", title: "Open Dev") { act("show") }
        Row(symbol: "chart.bar", title: "Usage") { act("open-usage") }
        Row(symbol: "person.2", title: "Switch Account…") { act("open-account") }
      }
      Divider().overlay(Look.line)
      VStack(spacing: 0) {
        Row(symbol: "arrow.clockwise", title: store.refreshing ? "Refreshing…" : "Refresh") { store.refresh() }
        Row(
          symbol: store.showInBar ? "checkmark.square" : "square",
          title: "Show usage in menu bar"
        ) { store.setShowInBar(!store.showInBar) }
        Row(symbol: "power", title: "Quit Hanzo Dev") { act("quit") }
      }
    }
    .padding(14)
    .frame(width: 340)
    .foregroundStyle(.white)
    .background(Look.background)
    .environment(\.colorScheme, .dark)
  }
}
