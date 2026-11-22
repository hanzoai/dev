import AppIntents
import SwiftUI
import WidgetKit
#if canImport(FoundationModels)
import FoundationModels
#endif

// MARK: - Live Hanzo Status & AI Brief Widget (App Group Data + iOS 26 FoundationModels)

private enum HanzoBriefStore {
  static let textKey = "hanzo_brief_latest_text"
  static let dateKey = "hanzo_brief_last_updated"

  static func save(_ text: String) {
    let defaults = HanzoWidgetDataStore.sharedDefaults()
    defaults.set(text, forKey: textKey)
    defaults.set(Date(), forKey: dateKey)
    UserDefaults.standard.set(text, forKey: textKey)
    UserDefaults.standard.set(Date(), forKey: dateKey)
  }

  static func load() -> (text: String?, date: Date) {
    let shared = HanzoWidgetDataStore.sharedDefaults()
    let text = shared.string(forKey: textKey) ?? UserDefaults.standard.string(forKey: textKey)
    let date = (shared.object(forKey: dateKey) as? Date)
      ?? (UserDefaults.standard.object(forKey: dateKey) as? Date)
      ?? Date()
    return (text, date)
  }
}

@available(iOS 17.0, *)
enum HanzoBriefService {
  static func generate(prompt: String, snapshot: HanzoWidgetSnapshotPayload) async -> String {
    let cleanPrompt = prompt.trimmingCharacters(in: .whitespacesAndNewlines)
    let contextLine = "Status: \(snapshot.connectionStatus). Model: \(snapshot.cloudModel). Recent threads: \(snapshot.recentThreads.prefix(3).map(\.title).joined(separator: ", "))."
    let effectivePrompt = cleanPrompt.isEmpty
      ? "Summarize my Hanzo workspace status concisely in one sentence. \(contextLine)"
      : "\(cleanPrompt)\n\nWorkspace context: \(contextLine)"

    #if canImport(FoundationModels)
    if #available(iOS 26.0, *) {
      do {
        let session = LanguageModelSession()
        let response = try await session.respond(to: effectivePrompt)
        let text = response.content.trimmingCharacters(in: .whitespacesAndNewlines)
        if !text.isEmpty {
          return text
        }
      } catch {
        // Fall through to live synced workspace summary
      }
    }
    #endif

    if let latest = snapshot.recentThreads.first {
      return "\(snapshot.connectionStatus) · Latest: \(latest.title)"
    }
    return snapshot.connectionStatus
  }
}

@available(iOS 17.0, *)
struct RefreshHanzoBriefIntent: AppIntent {
  static var title: LocalizedStringResource = "Refresh Hanzo AI Brief"
  static var description = IntentDescription("Refreshes your Hanzo AI brief directly on your Home Screen.")

  @Parameter(title: "Prompt", default: "")
  var prompt: String

  init() {}

  init(prompt: String) {
    self.prompt = prompt
  }

  func perform() async throws -> some IntentResult {
    let snapshot = HanzoWidgetDataStore.loadSnapshot()
    let result = await HanzoBriefService.generate(prompt: prompt, snapshot: snapshot)
    HanzoBriefStore.save(result)
    WidgetCenter.shared.reloadTimelines(ofKind: "HanzoBrief")
    return .result()
  }
}

@available(iOS 17.0, *)
struct HanzoBriefConfigurationIntent: WidgetConfigurationIntent {
  static var title: LocalizedStringResource = "Hanzo AI Brief"
  static var description = IntentDescription("Configure the prompt and theme for your Home Screen AI Brief.")

  @Parameter(title: "Prompt", default: "")
  var prompt: String

  @Parameter(title: "Theme", default: .zen)
  var theme: AskHanzoTheme
}

@available(iOS 17.0, *)
struct HanzoBriefEntry: TimelineEntry {
  let date: Date
  let text: String
  let snapshot: HanzoWidgetSnapshotPayload
  let configuration: HanzoBriefConfigurationIntent
}

@available(iOS 17.0, *)
struct HanzoBriefProvider: AppIntentTimelineProvider {
  private func makeEntry(configuration: HanzoBriefConfigurationIntent) -> HanzoBriefEntry {
    let snapshot = HanzoWidgetDataStore.loadSnapshot()
    let stored = HanzoBriefStore.load()
    let resolvedText: String
    if let saved = stored.text, !saved.isEmpty {
      resolvedText = saved
    } else if let latest = snapshot.recentThreads.first {
      resolvedText = "\(latest.title) — \(latest.preview)"
    } else {
      resolvedText = snapshot.connectionStatus
    }
    return HanzoBriefEntry(
      date: stored.date,
      text: resolvedText,
      snapshot: snapshot,
      configuration: configuration
    )
  }

  func placeholder(in context: Context) -> HanzoBriefEntry {
    makeEntry(configuration: HanzoBriefConfigurationIntent())
  }

  func snapshot(for configuration: HanzoBriefConfigurationIntent, in context: Context) async -> HanzoBriefEntry {
    makeEntry(configuration: configuration)
  }

  func timeline(for configuration: HanzoBriefConfigurationIntent, in context: Context) async -> Timeline<HanzoBriefEntry> {
    let entry = makeEntry(configuration: configuration)
    let nextUpdate = Calendar.current.date(byAdding: .minute, value: 30, to: Date()) ?? Date().addingTimeInterval(1800)
    return Timeline(entries: [entry], policy: .after(nextUpdate))
  }
}

@available(iOS 17.0, *)
struct HanzoBriefEntryView: View {
  @Environment(\.widgetFamily) private var family
  @Environment(\.colorScheme) private var colorScheme
  let entry: HanzoBriefEntry

  private var palette: AskHanzoPalette {
    AskHanzoPalette.resolve(theme: entry.configuration.theme, scheme: colorScheme)
  }

  var body: some View {
    VStack(alignment: .leading, spacing: 10) {
      HStack(alignment: .center) {
        HStack(spacing: 6) {
          Image("HanzoMark")
            .resizable()
            .renderingMode(.template)
            .aspectRatio(contentMode: .fit)
            .frame(width: 15, height: 15)
            .foregroundStyle(palette.titleColor)
          Text("Hanzo Brief")
            .font(.system(size: 12, weight: .semibold))
            .foregroundStyle(palette.titleColor)
        }

        Spacer(minLength: 4)

        Button(intent: RefreshHanzoBriefIntent(prompt: entry.configuration.prompt)) {
          Image(systemName: "arrow.clockwise")
            .font(.system(size: 11, weight: .semibold))
            .foregroundStyle(palette.iconColor)
            .frame(width: 26, height: 26)
            .background(
              Circle()
                .fill(palette.tileFill)
                .overlay(Circle().strokeBorder(palette.tileBorder, lineWidth: 0.7))
            )
        }
        .buttonStyle(.plain)
        .accessibilityLabel(Text("Refresh AI Brief"))
      }

      Text(entry.text)
        .font(.system(size: family == .systemSmall ? 13 : 14, weight: .regular))
        .foregroundStyle(palette.titleColor)
        .lineLimit(family == .systemLarge ? 6 : 4)
        .minimumScaleFactor(0.85)

      if family == .systemLarge, !entry.snapshot.recentThreads.isEmpty {
        VStack(alignment: .leading, spacing: 6) {
          Text("Recent Chats")
            .font(.system(size: 10, weight: .semibold))
            .foregroundStyle(palette.mutedColor)
            .textCase(.uppercase)
          ForEach(Array(entry.snapshot.recentThreads.prefix(3)), id: \.key) { thread in
            let encoded = thread.key.addingPercentEncoding(withAllowedCharacters: .urlPathAllowed) ?? thread.key
            Link(destination: URL(string: "hanzo://chat/thread/\(encoded)") ?? URL(string: "hanzo://chat/new")!) {
              HStack(spacing: 8) {
                Image(systemName: "bubble.left")
                  .font(.system(size: 11))
                  .foregroundStyle(palette.mutedColor)
                Text(thread.title)
                  .font(.system(size: 12, weight: .medium))
                  .foregroundStyle(palette.titleColor)
                  .lineLimit(1)
                Spacer(minLength: 4)
              }
              .padding(.horizontal, 10)
              .padding(.vertical, 7)
              .background(
                RoundedRectangle(cornerRadius: 10, style: .continuous)
                  .fill(palette.tileFill)
              )
            }
          }
        }
      }

      Spacer(minLength: 0)

      HStack(spacing: 8) {
        Circle()
          .fill(entry.snapshot.signedIn ? palette.onlineDot : palette.offlineDot)
          .frame(width: 6, height: 6)
        Text(entry.snapshot.connectionStatus)
          .font(.system(size: 10, weight: .medium))
          .foregroundStyle(palette.mutedColor)
          .lineLimit(1)
        Spacer(minLength: 4)
        Link(destination: URL(string: "hanzo://chat/new")!) {
          Text("Chat")
            .font(.system(size: 11, weight: .semibold))
            .foregroundStyle(palette.titleColor)
            .padding(.horizontal, 10)
            .padding(.vertical, 4)
            .background(
              Capsule(style: .continuous)
                .fill(palette.tileFill)
                .overlay(Capsule().strokeBorder(palette.tileBorder, lineWidth: 0.7))
            )
        }
      }
    }
    .padding(2)
    .widgetURL(URL(string: "hanzo://chat/new")!)
    .containerBackground(palette.background, for: .widget)
  }
}

@available(iOS 17.0, *)
struct HanzoBrief: Widget {
  let kind: String = "HanzoBrief"

  var body: some WidgetConfiguration {
    AppIntentConfiguration(kind: kind, intent: HanzoBriefConfigurationIntent.self, provider: HanzoBriefProvider()) { entry in
      HanzoBriefEntryView(entry: entry)
    }
    .configurationDisplayName("Hanzo AI Brief")
    .description("Live Hanzo workspace brief with one-tap refresh and chat across all sizes.")
    .supportedFamilies([.systemSmall, .systemMedium, .systemLarge])
  }
}
