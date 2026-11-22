import AppIntents
import SwiftUI
import WidgetKit

// MARK: - Configurable Theme & Quick Actions (Edit Widget on Home Screen)

@available(iOS 17.0, *)
enum AskHanzoTheme: String, CaseIterable, AppEnum {
  case zen
  case lelouch
  case crimson
  case auto

  static var typeDisplayRepresentation = TypeDisplayRepresentation(name: "Theme")

  static var caseDisplayRepresentations: [AskHanzoTheme: DisplayRepresentation] = [
    .zen: DisplayRepresentation(title: "Zen (Pure Black)"),
    .lelouch: DisplayRepresentation(title: "Lelouch (Obsidian)"),
    .crimson: DisplayRepresentation(title: "Crimson Glow"),
    .auto: DisplayRepresentation(title: "System"),
  ]
}

@available(iOS 17.0, *)
enum AskHanzoAction: String, CaseIterable, AppEnum {
  case camera
  case voice
  case attach
  case dev
  case chat
  case clipboard
  case models
  case usage
  case environments
  case settings

  static var typeDisplayRepresentation = TypeDisplayRepresentation(name: "Quick Action")

  static var caseDisplayRepresentations: [AskHanzoAction: DisplayRepresentation] = [
    .camera: DisplayRepresentation(title: "Camera", image: .init(systemName: "camera")),
    .voice: DisplayRepresentation(title: "Voice", image: .init(systemName: "mic")),
    .attach: DisplayRepresentation(title: "Attach File", image: .init(systemName: "paperclip")),
    .dev: DisplayRepresentation(title: "Hanzo Dev", image: .init(systemName: "terminal")),
    .chat: DisplayRepresentation(title: "New Chat", image: .init(systemName: "bubble.left")),
    .clipboard: DisplayRepresentation(title: "Ask Clipboard", image: .init(systemName: "doc.on.clipboard")),
    .models: DisplayRepresentation(title: "Models", image: .init(systemName: "sparkles")),
    .usage: DisplayRepresentation(title: "Usage", image: .init(systemName: "chart.bar")),
    .environments: DisplayRepresentation(title: "Machines", image: .init(systemName: "desktopcomputer")),
    .settings: DisplayRepresentation(title: "Settings", image: .init(systemName: "gearshape")),
  ]

  var symbol: String {
    switch self {
    case .camera: return "camera"
    case .voice: return "mic"
    case .attach: return "paperclip"
    case .dev: return "terminal"
    case .chat: return "bubble.left"
    case .clipboard: return "doc.on.clipboard"
    case .models: return "sparkles"
    case .usage: return "chart.bar"
    case .environments: return "desktopcomputer"
    case .settings: return "gearshape"
    }
  }

  var label: String {
    switch self {
    case .camera: return "Camera"
    case .voice: return "Voice"
    case .attach: return "Attach"
    case .dev: return "Dev"
    case .chat: return "Chat"
    case .clipboard: return "Clipboard"
    case .models: return "Models"
    case .usage: return "Usage"
    case .environments: return "Machines"
    case .settings: return "Settings"
    }
  }

  var url: URL {
    let raw: String
    switch self {
    case .camera: raw = "hanzodev://ask?action=camera"
    case .voice: raw = "hanzodev://ask?action=voice"
    case .attach: raw = "hanzodev://ask?action=attach"
    case .dev: raw = "hanzodev://new"
    case .chat: raw = "hanzodev://ask?action=chat"
    case .clipboard: raw = "hanzodev://ask?action=clipboard"
    case .models: raw = "hanzodev://chat/settings"
    case .usage: raw = "hanzodev://settings/usage"
    case .environments: raw = "hanzodev://settings/environments"
    case .settings: raw = "hanzodev://settings"
    }
    return URL(string: raw)!
  }
}

@available(iOS 17.0, *)
struct AskHanzoConfigurationIntent: WidgetConfigurationIntent {
  static var title: LocalizedStringResource = "Ask Hanzo"
  static var description = IntentDescription(
    "Customize the launcher headline, theme, and quick-action buttons."
  )

  @Parameter(title: "Headline", default: "Ask AI Anything")
  var headline: String

  @Parameter(title: "Theme", default: .zen)
  var theme: AskHanzoTheme

  @Parameter(title: "Button 1", default: .camera)
  var slot1: AskHanzoAction

  @Parameter(title: "Button 2", default: .voice)
  var slot2: AskHanzoAction

  @Parameter(title: "Button 3", default: .attach)
  var slot3: AskHanzoAction

  @Parameter(title: "Button 4", default: .dev)
  var slot4: AskHanzoAction
}

// MARK: - Timeline

@available(iOS 17.0, *)
struct AskHanzoEntry: TimelineEntry {
  let date: Date
  let configuration: AskHanzoConfigurationIntent
}

@available(iOS 17.0, *)
struct AskHanzoProvider: AppIntentTimelineProvider {
  func placeholder(in context: Context) -> AskHanzoEntry {
    AskHanzoEntry(date: Date(), configuration: AskHanzoConfigurationIntent())
  }

  func snapshot(for configuration: AskHanzoConfigurationIntent, in context: Context) async -> AskHanzoEntry {
    AskHanzoEntry(date: Date(), configuration: configuration)
  }

  func timeline(for configuration: AskHanzoConfigurationIntent, in context: Context) async -> Timeline<AskHanzoEntry> {
    Timeline(entries: [AskHanzoEntry(date: Date(), configuration: configuration)], policy: .never)
  }
}

// MARK: - Palette Helper

@available(iOS 17.0, *)
struct AskHanzoPalette {
  let background: AnyShapeStyle
  let titleColor: Color
  let mutedColor: Color
  let tileFill: Color
  let tileBorder: Color
  let iconColor: Color

  static func resolve(theme: AskHanzoTheme, scheme: ColorScheme) -> AskHanzoPalette {
    let isLight = theme == .auto && scheme == .light
    switch theme {
    case .zen:
      return AskHanzoPalette(
        background: AnyShapeStyle(Color.black),
        titleColor: Color.white.opacity(0.95),
        mutedColor: Color.white.opacity(0.55),
        tileFill: Color.white.opacity(0.08),
        tileBorder: Color.white.opacity(0.11),
        iconColor: Color.white.opacity(0.92)
      )
    case .lelouch:
      return AskHanzoPalette(
        background: AnyShapeStyle(
          LinearGradient(
            colors: [Color(red: 0.09, green: 0.09, blue: 0.10), Color(red: 0.05, green: 0.05, blue: 0.06)],
            startPoint: .topLeading,
            endPoint: .bottomTrailing
          )
        ),
        titleColor: Color.white.opacity(0.94),
        mutedColor: Color.white.opacity(0.54),
        tileFill: Color.white.opacity(0.075),
        tileBorder: Color.white.opacity(0.10),
        iconColor: Color.white.opacity(0.90)
      )
    case .crimson:
      return AskHanzoPalette(
        background: AnyShapeStyle(
          LinearGradient(
            colors: [
              Color(red: 0.10, green: 0.07, blue: 0.08),
              Color(red: 0.24, green: 0.06, blue: 0.10),
            ],
            startPoint: .topLeading,
            endPoint: .bottomTrailing
          )
        ),
        titleColor: Color.white.opacity(0.96),
        mutedColor: Color.white.opacity(0.62),
        tileFill: Color.white.opacity(0.09),
        tileBorder: Color.white.opacity(0.13),
        iconColor: Color.white.opacity(0.94)
      )
    case .auto:
      if isLight {
        return AskHanzoPalette(
          background: AnyShapeStyle(Color(UIColor.systemBackground)),
          titleColor: Color.black.opacity(0.92),
          mutedColor: Color.black.opacity(0.52),
          tileFill: Color.black.opacity(0.055),
          tileBorder: Color.black.opacity(0.09),
          iconColor: Color.black.opacity(0.86)
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

  private var headline: String {
    let trimmed = entry.configuration.headline.trimmingCharacters(in: .whitespacesAndNewlines)
    return trimmed.isEmpty ? "Ask AI Anything" : trimmed
  }

  private var actions: [AskHanzoAction] {
    [
      entry.configuration.slot1,
      entry.configuration.slot2,
      entry.configuration.slot3,
      entry.configuration.slot4,
    ]
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

  private func actionTile(_ action: AskHanzoAction, height: CGFloat = 54, showLabel: Bool = false) -> some View {
    Link(destination: action.url) {
      VStack(spacing: 4) {
        Image(systemName: action.symbol)
          .font(.system(size: showLabel ? 17 : 19, weight: .medium))
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
        RoundedRectangle(cornerRadius: 14, style: .continuous)
          .fill(palette.tileFill)
          .overlay(
            RoundedRectangle(cornerRadius: 14, style: .continuous)
              .strokeBorder(palette.tileBorder, lineWidth: 0.75)
          )
      )
    }
    .accessibilityLabel(Text(action.label))
  }

  // Medium: Matches the clean "Ask AI Anything" launcher bar with 4 configurable tiles
  private var mediumView: some View {
    VStack(alignment: .leading, spacing: 14) {
      HStack(alignment: .center) {
        Text(headline)
          .font(.system(size: 16, weight: .medium))
          .foregroundStyle(palette.titleColor)
          .lineLimit(1)
        Spacer(minLength: 8)
        markView
          .frame(width: 22, height: 22)
      }
      .padding(.top, 2)

      Spacer(minLength: 0)

      HStack(spacing: 10) {
        ForEach(Array(actions.enumerated()), id: \.offset) { _, action in
          actionTile(action, height: 56)
        }
      }
    }
    .padding(2)
    .widgetURL(URL(string: "hanzodev://ask?action=chat")!)
    .containerBackground(palette.background, for: .widget)
  }

  // Small: Compact launcher + 2 top quick actions
  private var smallView: some View {
    VStack(alignment: .leading, spacing: 12) {
      HStack(alignment: .center) {
        markView
          .frame(width: 18, height: 18)
        Spacer(minLength: 4)
        Text("Hanzo")
          .font(.system(size: 11, weight: .semibold))
          .foregroundStyle(palette.mutedColor)
      }

      Text(headline)
        .font(.system(size: 14, weight: .medium))
        .foregroundStyle(palette.titleColor)
        .lineLimit(2)

      Spacer(minLength: 0)

      HStack(spacing: 8) {
        actionTile(entry.configuration.slot1, height: 42)
        actionTile(entry.configuration.slot2, height: 42)
      }
    }
    .padding(2)
    .widgetURL(URL(string: "hanzodev://ask?action=chat")!)
    .containerBackground(palette.background, for: .widget)
  }

  // Large: Full mobile AI command launcher + 4 tiles + one-tap prompt starters
  private var largeView: some View {
    VStack(alignment: .leading, spacing: 14) {
      HStack(alignment: .center) {
        Text(headline)
          .font(.system(size: 18, weight: .semibold))
          .foregroundStyle(palette.titleColor)
          .lineLimit(1)
        Spacer(minLength: 8)
        markView
          .frame(width: 22, height: 22)
      }

      HStack(spacing: 10) {
        ForEach(Array(actions.enumerated()), id: \.offset) { _, action in
          actionTile(action, height: 58, showLabel: true)
        }
      }

      VStack(alignment: .leading, spacing: 8) {
        Text("Quick Prompts")
          .font(.system(size: 11, weight: .semibold))
          .foregroundStyle(palette.mutedColor)
          .textCase(.uppercase)

        promptRow(
          icon: "sparkles",
          title: "Brainstorm architecture & ideas",
          url: URL(string: "hanzodev://ask?prompt=Help%20me%20brainstorm%20practical%20options%20for%20")!
        )
        promptRow(
          icon: "chevron.left.forwardslash.chevron.right",
          title: "Review code & find edge cases",
          url: URL(string: "hanzodev://ask?prompt=Review%20this%20code%20for%20bugs%20and%20edge%20cases:%20")!
        )
        promptRow(
          icon: "doc.on.clipboard",
          title: "Summarize what's on my clipboard",
          url: URL(string: "hanzodev://ask?action=clipboard")!
        )
      }

      Spacer(minLength: 0)
    }
    .padding(2)
    .widgetURL(URL(string: "hanzodev://ask?action=chat")!)
    .containerBackground(palette.background, for: .widget)
  }

  private func promptRow(icon: String, title: String, url: URL) -> some View {
    Link(destination: url) {
      HStack(spacing: 10) {
        Image(systemName: icon)
          .font(.system(size: 13, weight: .medium))
          .foregroundStyle(palette.mutedColor)
          .frame(width: 18)
        Text(title)
          .font(.system(size: 13, weight: .medium))
          .foregroundStyle(palette.titleColor)
          .lineLimit(1)
        Spacer(minLength: 4)
        Image(systemName: "arrow.up.right")
          .font(.system(size: 11, weight: .semibold))
          .foregroundStyle(palette.mutedColor)
      }
      .padding(.horizontal, 12)
      .padding(.vertical, 10)
      .background(
        RoundedRectangle(cornerRadius: 12, style: .continuous)
          .fill(palette.tileFill)
          .overlay(
            RoundedRectangle(cornerRadius: 12, style: .continuous)
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
    .widgetURL(URL(string: "hanzodev://ask?action=voice")!)
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
      Text(headline)
        .font(.caption)
        .foregroundStyle(.secondary)
        .lineLimit(1)
    }
    .widgetURL(URL(string: "hanzodev://ask?action=chat")!)
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
    .description("Configurable AI launcher with one-tap Camera, Voice, Attach, and Dev shortcuts.")
    .supportedFamilies([.systemSmall, .systemMedium, .systemLarge, .accessoryCircular, .accessoryRectangular])
  }
}
