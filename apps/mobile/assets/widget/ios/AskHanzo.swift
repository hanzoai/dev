import AppIntents
import SwiftUI
import WidgetKit

// MARK: - Shared Live Hanzo Data Store (App Group + standard UserDefaults fallback)

struct HanzoWidgetMachinePayload: Codable, Hashable {
  let id: String
  let name: String
  let online: Bool
  let kind: String
}

struct HanzoWidgetThreadPayload: Codable, Hashable {
  let key: String
  let title: String
  let preview: String
  let updatedAt: Double
  let pinned: Bool?
}

struct HanzoWidgetSnapshotPayload: Codable {
  let signedIn: Bool
  let accountName: String?
  let accountEmail: String?
  let cloudAvailable: Bool
  let cloudModel: String
  let connectionStatus: String
  let environmentsCount: Int
  let onlineMachinesCount: Int
  let machines: [HanzoWidgetMachinePayload]
  let recentThreads: [HanzoWidgetThreadPayload]
  let updatedAt: Double

  static let empty = HanzoWidgetSnapshotPayload(
    signedIn: false,
    accountName: nil,
    accountEmail: nil,
    cloudAvailable: false,
    cloudModel: "enso-flash",
    connectionStatus: "Open Hanzo to sign in",
    environmentsCount: 0,
    onlineMachinesCount: 0,
    machines: [],
    recentThreads: [],
    updatedAt: 0
  )
}

enum HanzoWidgetDataStore {
  static let snapshotKey = "hanzo_widget_snapshot_v1"

  static func appGroupIdentifier() -> String? {
    guard let raw = Bundle.main.bundleIdentifier, !raw.isEmpty else {
      return nil
    }
    let mainBundleId: String
    if raw.hasSuffix(".widgets") {
      mainBundleId = String(raw.dropLast(".widgets".count))
    } else if raw.hasSuffix(".sharing") {
      mainBundleId = String(raw.dropLast(".sharing".count))
    } else {
      mainBundleId = raw
    }
    return "group.\(mainBundleId)"
  }

  static func sharedDefaults() -> UserDefaults {
    if let groupId = appGroupIdentifier(), let shared = UserDefaults(suiteName: groupId) {
      return shared
    }
    return UserDefaults.standard
  }

  static func loadSnapshot() -> HanzoWidgetSnapshotPayload {
    let candidates: [UserDefaults] = {
      var list: [UserDefaults] = []
      if let groupId = appGroupIdentifier(), let shared = UserDefaults(suiteName: groupId) {
        list.append(shared)
      }
      list.append(UserDefaults.standard)
      return list
    }()

    let decoder = JSONDecoder()
    for defaults in candidates {
      if let raw = defaults.string(forKey: snapshotKey),
         let data = raw.data(using: .utf8),
         let decoded = try? decoder.decode(HanzoWidgetSnapshotPayload.self, from: data) {
        return decoded
      }
    }
    return .empty
  }

  static func formatRelativeTime(_ epochMs: Double) -> String {
    guard epochMs > 0 else { return "" }
    let date = Date(timeIntervalSince1970: epochMs / 1000.0)
    let seconds = max(0, Int(Date().timeIntervalSince(date)))
    if seconds < 60 { return "now" }
    let minutes = seconds / 60
    if minutes < 60 { return "\(minutes)m" }
    let hours = minutes / 60
    if hours < 24 { return "\(hours)h" }
    let days = hours / 24
    return "\(days)d"
  }
}

// MARK: - Configurable Theme & Quick Actions (Edit Widget on Home Screen)

@available(iOS 17.0, *)
enum AskHanzoTheme: String, CaseIterable, AppEnum {
  case zen
  case obsidian
  case crimson
  case auto

  static var typeDisplayRepresentation = TypeDisplayRepresentation(name: "Theme")

  static var caseDisplayRepresentations: [AskHanzoTheme: DisplayRepresentation] = [
    .zen: DisplayRepresentation(title: "Hanzo Black (#000000)"),
    .obsidian: DisplayRepresentation(title: "Obsidian"),
    .crimson: DisplayRepresentation(title: "Crimson Glow"),
    .auto: DisplayRepresentation(title: "System"),
  ]
}

@available(iOS 17.0, *)
enum AskHanzoAction: String, CaseIterable, AppEnum {
  case chat
  case voice
  case camera
  case dev
  case attach
  case clipboard
  case models
  case usage
  case environments
  case permissions
  case settings

  static var typeDisplayRepresentation = TypeDisplayRepresentation(name: "Quick Action")

  static var caseDisplayRepresentations: [AskHanzoAction: DisplayRepresentation] = [
    .chat: DisplayRepresentation(title: "New Chat", image: .init(systemName: "bubble.left.fill")),
    .voice: DisplayRepresentation(title: "Voice Chat", image: .init(systemName: "mic.fill")),
    .camera: DisplayRepresentation(title: "Camera", image: .init(systemName: "camera")),
    .dev: DisplayRepresentation(title: "Hanzo Dev", image: .init(systemName: "terminal")),
    .attach: DisplayRepresentation(title: "Attach File", image: .init(systemName: "paperclip")),
    .clipboard: DisplayRepresentation(title: "Ask Clipboard", image: .init(systemName: "doc.on.clipboard")),
    .models: DisplayRepresentation(title: "Models", image: .init(systemName: "sparkles")),
    .usage: DisplayRepresentation(title: "Usage", image: .init(systemName: "chart.bar")),
    .environments: DisplayRepresentation(title: "Environments", image: .init(systemName: "desktopcomputer")),
    .permissions: DisplayRepresentation(title: "AI Permissions", image: .init(systemName: "hand.raised")),
    .settings: DisplayRepresentation(title: "Settings", image: .init(systemName: "gearshape")),
  ]

  var symbol: String {
    switch self {
    case .chat: return "bubble.left.fill"
    case .voice: return "mic.fill"
    case .camera: return "camera"
    case .dev: return "terminal"
    case .attach: return "paperclip"
    case .clipboard: return "doc.on.clipboard"
    case .models: return "sparkles"
    case .usage: return "chart.bar"
    case .environments: return "desktopcomputer"
    case .permissions: return "hand.raised"
    case .settings: return "gearshape"
    }
  }

  var label: String {
    switch self {
    case .chat: return "Chat"
    case .voice: return "Voice"
    case .camera: return "Vision"
    case .dev: return "Dev"
    case .attach: return "Attach"
    case .clipboard: return "Clipboard"
    case .models: return "Models"
    case .usage: return "Usage"
    case .environments: return "Machines"
    case .permissions: return "Permissions"
    case .settings: return "Settings"
    }
  }

  var url: URL {
    let raw: String
    switch self {
    case .chat: raw = "hanzo://chat/new"
    case .voice: raw = "hanzo://chat/voice"
    case .camera: raw = "hanzo://ask?action=camera"
    case .dev: raw = "hanzo://new"
    case .attach: raw = "hanzo://ask?action=attach"
    case .clipboard: raw = "hanzo://ask?action=clipboard"
    case .models: raw = "hanzo://chat/settings"
    case .usage: raw = "hanzo://settings/usage"
    case .environments: raw = "hanzo://settings/environments"
    case .permissions: raw = "hanzo://settings/permissions"
    case .settings: raw = "hanzo://settings"
    }
    return URL(string: raw)!
  }
}

@available(iOS 17.0, *)
struct AskHanzoConfigurationIntent: WidgetConfigurationIntent {
  static var title: LocalizedStringResource = "Hanzo"
  static var description = IntentDescription(
    "Live Hanzo Cloud & desktop status, recent chats, and one-tap chat shortcuts."
  )

  @Parameter(title: "Theme", default: .zen)
  var theme: AskHanzoTheme

  @Parameter(title: "Button 1", default: .chat)
  var slot1: AskHanzoAction

  @Parameter(title: "Button 2", default: .voice)
  var slot2: AskHanzoAction

  @Parameter(title: "Button 3", default: .camera)
  var slot3: AskHanzoAction

  @Parameter(title: "Button 4", default: .dev)
  var slot4: AskHanzoAction
}

// MARK: - Timeline

@available(iOS 17.0, *)
struct AskHanzoEntry: TimelineEntry {
  let date: Date
  let snapshot: HanzoWidgetSnapshotPayload
  let configuration: AskHanzoConfigurationIntent
}

@available(iOS 17.0, *)
struct AskHanzoProvider: AppIntentTimelineProvider {
  func placeholder(in context: Context) -> AskHanzoEntry {
    AskHanzoEntry(
      date: Date(),
      snapshot: HanzoWidgetDataStore.loadSnapshot(),
      configuration: AskHanzoConfigurationIntent()
    )
  }

  func snapshot(for configuration: AskHanzoConfigurationIntent, in context: Context) async -> AskHanzoEntry {
    AskHanzoEntry(
      date: Date(),
      snapshot: HanzoWidgetDataStore.loadSnapshot(),
      configuration: configuration
    )
  }

  func timeline(for configuration: AskHanzoConfigurationIntent, in context: Context) async -> Timeline<AskHanzoEntry> {
    let entry = AskHanzoEntry(
      date: Date(),
      snapshot: HanzoWidgetDataStore.loadSnapshot(),
      configuration: configuration
    )
    let nextUpdate = Calendar.current.date(byAdding: .minute, value: 15, to: Date()) ?? Date().addingTimeInterval(900)
    return Timeline(entries: [entry], policy: .after(nextUpdate))
  }
}

// MARK: - Palette Helper (Default: Pure Black #000000 matching the Hanzo App Icon)

@available(iOS 17.0, *)
struct AskHanzoPalette {
  let background: AnyShapeStyle
  let surfaceColor: Color
  let titleColor: Color
  let mutedColor: Color
  let tileFill: Color
  let tileBorder: Color
  let iconColor: Color
  let onlineDot: Color
  let offlineDot: Color

  static func resolve(theme: AskHanzoTheme, scheme: ColorScheme) -> AskHanzoPalette {
    let isLight = theme == .auto && scheme == .light
    switch theme {
    case .zen:
      return AskHanzoPalette(
        background: AnyShapeStyle(Color.black),
        surfaceColor: Color.black,
        titleColor: Color.white.opacity(0.96),
        mutedColor: Color.white.opacity(0.58),
        tileFill: Color.white.opacity(0.09),
        tileBorder: Color.white.opacity(0.14),
        iconColor: Color.white.opacity(0.95),
        onlineDot: Color(red: 0.20, green: 0.84, blue: 0.45),
        offlineDot: Color.white.opacity(0.30)
      )
    case .obsidian:
      return AskHanzoPalette(
        background: AnyShapeStyle(
          LinearGradient(
            colors: [Color.black, Color(red: 0.06, green: 0.06, blue: 0.07)],
            startPoint: .topLeading,
            endPoint: .bottomTrailing
          )
        ),
        surfaceColor: Color.black,
        titleColor: Color.white.opacity(0.95),
        mutedColor: Color.white.opacity(0.56),
        tileFill: Color.white.opacity(0.08),
        tileBorder: Color.white.opacity(0.12),
        iconColor: Color.white.opacity(0.92),
        onlineDot: Color(red: 0.20, green: 0.84, blue: 0.45),
        offlineDot: Color.white.opacity(0.28)
      )
    case .crimson:
      return AskHanzoPalette(
        background: AnyShapeStyle(
          LinearGradient(
            colors: [
              Color.black,
              Color(red: 0.20, green: 0.04, blue: 0.08),
            ],
            startPoint: .topLeading,
            endPoint: .bottomTrailing
          )
        ),
        surfaceColor: Color.black,
        titleColor: Color.white.opacity(0.96),
        mutedColor: Color.white.opacity(0.62),
        tileFill: Color.white.opacity(0.09),
        tileBorder: Color.white.opacity(0.14),
        iconColor: Color.white.opacity(0.95),
        onlineDot: Color(red: 0.20, green: 0.84, blue: 0.45),
        offlineDot: Color.white.opacity(0.30)
      )
    case .auto:
      if isLight {
        return AskHanzoPalette(
          background: AnyShapeStyle(Color(UIColor.systemBackground)),
          surfaceColor: Color(UIColor.systemBackground),
          titleColor: Color.black.opacity(0.92),
          mutedColor: Color.black.opacity(0.52),
          tileFill: Color.black.opacity(0.06),
          tileBorder: Color.black.opacity(0.10),
          iconColor: Color.black.opacity(0.88),
          onlineDot: Color(red: 0.14, green: 0.72, blue: 0.36),
          offlineDot: Color.black.opacity(0.25)
        )
      }
      return AskHanzoPalette.resolve(theme: .zen, scheme: .dark)
    }
  }
}

// MARK: - Views

@available(iOS 17.0, *)
struct AskHanzoEntryView: View {
  @Environment(\.widgetFamily) private var family
  @Environment(\.colorScheme) private var colorScheme
  let entry: AskHanzoEntry

  private var palette: AskHanzoPalette {
    AskHanzoPalette.resolve(theme: entry.configuration.theme, scheme: colorScheme)
  }

  private var snapshot: HanzoWidgetSnapshotPayload {
    entry.snapshot
  }

  private var actions: [AskHanzoAction] {
    [
      entry.configuration.slot1,
      entry.configuration.slot2,
      entry.configuration.slot3,
      entry.configuration.slot4,
    ]
  }

  private var statusLabel: String {
    if !snapshot.connectionStatus.isEmpty {
      return snapshot.connectionStatus
    }
    return snapshot.signedIn ? "Hanzo Cloud · Ready" : "Open Hanzo to sign in"
  }

  private var isConnected: Bool {
    snapshot.signedIn || snapshot.onlineMachinesCount > 0
  }

  private func threadURL(_ key: String) -> URL {
    let encoded = key.addingPercentEncoding(withAllowedCharacters: .urlPathAllowed) ?? key
    return URL(string: "hanzo://chat/thread/\(encoded)") ?? URL(string: "hanzo://chat/new")!
  }

  var body: some View {
    switch family {
    case .accessoryCircular:
      accessoryCircularView
    case .accessoryRectangular:
      accessoryRectangularView
    case .systemSmall:
      smallView
    case .systemLarge:
      largeView
    default:
      mediumView
    }
  }

  private var markView: some View {
    Image("HanzoMark")
      .resizable()
      .renderingMode(.template)
      .aspectRatio(contentMode: .fit)
      .foregroundStyle(palette.titleColor)
  }

  private var statusPill: some View {
    HStack(spacing: 5) {
      Circle()
        .fill(isConnected ? palette.onlineDot : palette.offlineDot)
        .frame(width: 6, height: 6)
      Text(statusLabel)
        .font(.system(size: 11, weight: .medium))
        .foregroundStyle(palette.mutedColor)
        .lineLimit(1)
    }
  }

  private func actionTile(_ action: AskHanzoAction, height: CGFloat = 44, showLabel: Bool = false) -> some View {
    Link(destination: action.url) {
      VStack(spacing: 3) {
        Image(systemName: action.symbol)
          .font(.system(size: showLabel ? 15 : 17, weight: .medium))
          .foregroundStyle(palette.iconColor)
        if showLabel {
          Text(action.label)
            .font(.system(size: 10, weight: .medium))
            .foregroundStyle(palette.mutedColor)
            .lineLimit(1)
        }
      }
      .frame(maxWidth: .infinity)
      .frame(height: height)
      .background(
        RoundedRectangle(cornerRadius: 13, style: .continuous)
          .fill(palette.tileFill)
          .overlay(
            RoundedRectangle(cornerRadius: 13, style: .continuous)
              .strokeBorder(palette.tileBorder, lineWidth: 0.75)
          )
      )
    }
    .accessibilityLabel(Text(action.label))
  }

  private var chatBarLink: some View {
    Link(destination: URL(string: "hanzo://chat/new")!) {
      HStack(spacing: 8) {
        Image(systemName: "bubble.left.fill")
          .font(.system(size: 12, weight: .semibold))
          .foregroundStyle(palette.iconColor)
        Text(snapshot.signedIn ? "Ask Hanzo (\(snapshot.cloudModel))…" : "Ask Hanzo…")
          .font(.system(size: 13, weight: .medium))
          .foregroundStyle(palette.mutedColor)
          .lineLimit(1)
        Spacer(minLength: 4)
        Image(systemName: "arrow.up.circle.fill")
          .font(.system(size: 18, weight: .semibold))
          .foregroundStyle(palette.titleColor)
      }
      .padding(.horizontal, 12)
      .padding(.vertical, 9)
      .background(
        Capsule(style: .continuous)
          .fill(palette.tileFill)
          .overlay(
            Capsule(style: .continuous)
              .strokeBorder(palette.tileBorder, lineWidth: 0.8)
          )
      )
    }
  }

  // MARK: Small (2x2): Pure black icon-matched card with live status, latest chat, and Chat/Voice buttons
  private var smallView: some View {
    VStack(alignment: .leading, spacing: 8) {
      HStack(alignment: .center, spacing: 6) {
        markView
          .frame(width: 17, height: 17)
        Text("Hanzo")
          .font(.system(size: 13, weight: .bold))
          .foregroundStyle(palette.titleColor)
        Spacer(minLength: 4)
        Circle()
          .fill(isConnected ? palette.onlineDot : palette.offlineDot)
          .frame(width: 7, height: 7)
      }

      if let latest = snapshot.recentThreads.first {
        Link(destination: threadURL(latest.key)) {
          VStack(alignment: .leading, spacing: 2) {
            Text(latest.title)
              .font(.system(size: 12, weight: .semibold))
              .foregroundStyle(palette.titleColor)
              .lineLimit(2)
            Text(statusLabel)
              .font(.system(size: 10, weight: .medium))
              .foregroundStyle(palette.mutedColor)
              .lineLimit(1)
          }
          .frame(maxWidth: .infinity, alignment: .leading)
        }
      } else {
        VStack(alignment: .leading, spacing: 2) {
          Text(snapshot.signedIn ? "Ask Hanzo" : "Connect Hanzo")
            .font(.system(size: 13, weight: .semibold))
            .foregroundStyle(palette.titleColor)
            .lineLimit(1)
          Text(statusLabel)
            .font(.system(size: 10, weight: .medium))
            .foregroundStyle(palette.mutedColor)
            .lineLimit(2)
        }
      }

      Spacer(minLength: 0)

      HStack(spacing: 8) {
        actionTile(entry.configuration.slot1, height: 38)
        actionTile(entry.configuration.slot2, height: 38)
      }
    }
    .padding(2)
    .widgetURL(URL(string: "hanzo://chat/new")!)
    .containerBackground(palette.background, for: .widget)
  }

  // MARK: Medium (4x2): Pure black bar with live Cloud & Machines status, latest chat, and interactive chat/action row
  private var mediumView: some View {
    VStack(alignment: .leading, spacing: 10) {
      HStack(alignment: .center, spacing: 8) {
        markView
          .frame(width: 18, height: 18)
        VStack(alignment: .leading, spacing: 1) {
          Text("Hanzo")
            .font(.system(size: 14, weight: .bold))
            .foregroundStyle(palette.titleColor)
          statusPill
        }
        Spacer(minLength: 4)
        Button(intent: RefreshHanzoWidgetIntent()) {
          Image(systemName: "arrow.clockwise")
            .font(.system(size: 11, weight: .semibold))
            .foregroundStyle(palette.mutedColor)
            .frame(width: 26, height: 26)
            .background(
              Circle()
                .fill(palette.tileFill)
                .overlay(Circle().strokeBorder(palette.tileBorder, lineWidth: 0.7))
            )
        }
        .buttonStyle(.plain)
        .accessibilityLabel(Text("Refresh Hanzo widget"))
      }

      if let latest = snapshot.recentThreads.first {
        Link(destination: threadURL(latest.key)) {
          HStack(spacing: 8) {
            Image(systemName: "bubble.left.and.bubble.right")
              .font(.system(size: 12, weight: .medium))
              .foregroundStyle(palette.mutedColor)
            Text(latest.title)
              .font(.system(size: 13, weight: .medium))
              .foregroundStyle(palette.titleColor)
              .lineLimit(1)
            Spacer(minLength: 4)
            let rel = HanzoWidgetDataStore.formatRelativeTime(latest.updatedAt)
            if !rel.isEmpty {
              Text(rel)
                .font(.system(size: 10, weight: .medium))
                .foregroundStyle(palette.mutedColor)
            }
          }
          .padding(.horizontal, 10)
          .padding(.vertical, 7)
          .background(
            RoundedRectangle(cornerRadius: 11, style: .continuous)
              .fill(palette.tileFill)
              .overlay(
                RoundedRectangle(cornerRadius: 11, style: .continuous)
                  .strokeBorder(palette.tileBorder, lineWidth: 0.6)
              )
          )
        }
      } else {
        chatBarLink
      }

      HStack(spacing: 8) {
        ForEach(Array(actions.enumerated()), id: \.offset) { _, action in
          actionTile(action, height: 42)
        }
      }
    }
    .padding(2)
    .widgetURL(URL(string: "hanzo://chat/new")!)
    .containerBackground(palette.background, for: .widget)
  }

  // MARK: Large (4x4): Full Hanzo Command Center on pure #000000 with live environments, recent chats & direct chat bar
  private var largeView: some View {
    VStack(alignment: .leading, spacing: 11) {
      HStack(alignment: .center, spacing: 8) {
        markView
          .frame(width: 20, height: 20)
        VStack(alignment: .leading, spacing: 1) {
          Text(snapshot.accountName ?? "Hanzo")
            .font(.system(size: 16, weight: .bold))
            .foregroundStyle(palette.titleColor)
            .lineLimit(1)
          statusPill
        }
        Spacer(minLength: 6)
        Button(intent: RefreshHanzoWidgetIntent()) {
          Image(systemName: "arrow.clockwise")
            .font(.system(size: 12, weight: .semibold))
            .foregroundStyle(palette.mutedColor)
            .frame(width: 28, height: 28)
            .background(
              Circle()
                .fill(palette.tileFill)
                .overlay(Circle().strokeBorder(palette.tileBorder, lineWidth: 0.7))
            )
        }
        .buttonStyle(.plain)
        .accessibilityLabel(Text("Refresh Hanzo widget"))
      }

      chatBarLink

      HStack(spacing: 8) {
        ForEach(Array(actions.enumerated()), id: \.offset) { _, action in
          actionTile(action, height: 48, showLabel: true)
        }
      }

      // Live Environments (Hanzo Cloud + linked desktop machines)
      if !snapshot.machines.isEmpty {
        Link(destination: URL(string: "hanzo://settings/environments")!) {
          HStack(spacing: 6) {
            ForEach(Array(snapshot.machines.prefix(3)), id: \.id) { machine in
              HStack(spacing: 5) {
                Circle()
                  .fill(machine.online ? palette.onlineDot : palette.offlineDot)
                  .frame(width: 6, height: 6)
                Text(machine.name)
                  .font(.system(size: 11, weight: .medium))
                  .foregroundStyle(palette.titleColor)
                  .lineLimit(1)
              }
              .padding(.horizontal, 8)
              .padding(.vertical, 5)
              .background(
                Capsule(style: .continuous)
                  .fill(palette.tileFill)
                  .overlay(
                    Capsule(style: .continuous)
                      .strokeBorder(palette.tileBorder, lineWidth: 0.6)
                  )
              )
            }
            Spacer(minLength: 0)
          }
        }
      }

      // Live Recent Chats (tap to open thread immediately) or direct quick chat actions
      VStack(alignment: .leading, spacing: 6) {
        HStack {
          Text(snapshot.recentThreads.isEmpty ? "Start Chatting" : "Recent Chats")
            .font(.system(size: 10, weight: .semibold))
            .foregroundStyle(palette.mutedColor)
            .textCase(.uppercase)
          Spacer(minLength: 4)
          if snapshot.environmentsCount > 0 {
            Text("\(snapshot.environmentsCount) env\(snapshot.environmentsCount == 1 ? "" : "s")")
              .font(.system(size: 10, weight: .medium))
              .foregroundStyle(palette.mutedColor)
          }
        }

        if snapshot.recentThreads.isEmpty {
          promptRow(
            icon: "sparkles",
            title: "Ask Hanzo Cloud anything",
            subtitle: snapshot.cloudModel,
            url: URL(string: "hanzo://chat/new")!
          )
          promptRow(
            icon: "mic.fill",
            title: "Start hands-free voice chat",
            subtitle: "Voice dictation",
            url: URL(string: "hanzo://chat/voice")!
          )
          promptRow(
            icon: "desktopcomputer",
            title: "View Environments & Macs",
            subtitle: statusLabel,
            url: URL(string: "hanzo://settings/environments")!
          )
        } else {
          ForEach(Array(snapshot.recentThreads.prefix(3)), id: \.key) { thread in
            threadRow(thread)
          }
        }
      }

      Spacer(minLength: 0)
    }
    .padding(2)
    .widgetURL(URL(string: "hanzo://chat/new")!)
    .containerBackground(palette.background, for: .widget)
  }

  private func threadRow(_ thread: HanzoWidgetThreadPayload) -> some View {
    Link(destination: threadURL(thread.key)) {
      HStack(spacing: 9) {
        Image(systemName: thread.pinned == true ? "pin.fill" : "bubble.left")
          .font(.system(size: 12, weight: .medium))
          .foregroundStyle(palette.mutedColor)
          .frame(width: 16)
        VStack(alignment: .leading, spacing: 1) {
          Text(thread.title)
            .font(.system(size: 12, weight: .semibold))
            .foregroundStyle(palette.titleColor)
            .lineLimit(1)
          if !thread.preview.isEmpty {
            Text(thread.preview)
              .font(.system(size: 10, weight: .regular))
              .foregroundStyle(palette.mutedColor)
              .lineLimit(1)
          }
        }
        Spacer(minLength: 4)
        let rel = HanzoWidgetDataStore.formatRelativeTime(thread.updatedAt)
        if !rel.isEmpty {
          Text(rel)
            .font(.system(size: 10, weight: .medium))
            .foregroundStyle(palette.mutedColor)
        }
      }
      .padding(.horizontal, 10)
      .padding(.vertical, 7)
      .background(
        RoundedRectangle(cornerRadius: 11, style: .continuous)
          .fill(palette.tileFill)
          .overlay(
            RoundedRectangle(cornerRadius: 11, style: .continuous)
              .strokeBorder(palette.tileBorder, lineWidth: 0.6)
          )
      )
    }
  }

  private func promptRow(icon: String, title: String, subtitle: String, url: URL) -> some View {
    Link(destination: url) {
      HStack(spacing: 9) {
        Image(systemName: icon)
          .font(.system(size: 12, weight: .medium))
          .foregroundStyle(palette.iconColor)
          .frame(width: 16)
        VStack(alignment: .leading, spacing: 1) {
          Text(title)
            .font(.system(size: 12, weight: .semibold))
            .foregroundStyle(palette.titleColor)
            .lineLimit(1)
          Text(subtitle)
            .font(.system(size: 10, weight: .regular))
            .foregroundStyle(palette.mutedColor)
            .lineLimit(1)
        }
        Spacer(minLength: 4)
        Image(systemName: "arrow.up.right")
          .font(.system(size: 10, weight: .semibold))
          .foregroundStyle(palette.mutedColor)
      }
      .padding(.horizontal, 10)
      .padding(.vertical, 7)
      .background(
        RoundedRectangle(cornerRadius: 11, style: .continuous)
          .fill(palette.tileFill)
          .overlay(
            RoundedRectangle(cornerRadius: 11, style: .continuous)
              .strokeBorder(palette.tileBorder, lineWidth: 0.6)
          )
      )
    }
  }

  private var accessoryCircularView: some View {
    ZStack {
      AccessoryWidgetBackground()
      Image("HanzoMark")
        .resizable()
        .renderingMode(.template)
        .aspectRatio(contentMode: .fit)
        .frame(width: 24, height: 24)
    }
    .widgetURL(URL(string: "hanzo://chat/voice")!)
    .containerBackground(.clear, for: .widget)
  }

  private var accessoryRectangularView: some View {
    VStack(alignment: .leading, spacing: 2) {
      HStack(spacing: 5) {
        Image("HanzoMark")
          .resizable()
          .renderingMode(.template)
          .aspectRatio(contentMode: .fit)
          .frame(width: 13, height: 13)
        Text("Hanzo")
          .font(.caption.bold())
      }
      Text(snapshot.recentThreads.first?.title ?? statusLabel)
        .font(.caption)
        .foregroundStyle(.secondary)
        .lineLimit(1)
    }
    .widgetURL(URL(string: "hanzo://chat/new")!)
    .containerBackground(.clear, for: .widget)
  }
}

// MARK: - Widget Definition

@available(iOS 17.0, *)
struct AskHanzo: Widget {
  let kind: String = "AskHanzo"

  var body: some WidgetConfiguration {
    AppIntentConfiguration(kind: kind, intent: AskHanzoConfigurationIntent.self, provider: AskHanzoProvider()) { entry in
      AskHanzoEntryView(entry: entry)
    }
    .configurationDisplayName("Ask Hanzo")
    .description("Live Hanzo Cloud & desktop status, recent chats, and one-tap chat on pure black.")
    .supportedFamilies([.systemSmall, .systemMedium, .systemLarge, .accessoryCircular, .accessoryRectangular])
  }
}
