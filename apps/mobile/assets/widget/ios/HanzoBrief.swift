import AppIntents
import SwiftUI
import WidgetKit
#if canImport(FoundationModels)
import FoundationModels
#endif

// MARK: - Interactive On-Device AI Brief Widget (iOS 26 FoundationModels + fallback)

private enum HanzoBriefStore {
  static let textKey = "hanzo_brief_latest_text"
  static let dateKey = "hanzo_brief_last_updated"
  static let indexKey = "hanzo_brief_fallback_index"

  static let fallbacks: [String] = [
    "Ship the smallest end-to-end slice first; verify on real hardware before expanding scope.",
    "Delete one indirection or stale flag today so every code path has one source of truth.",
    "Batch independent reads and edits together; let tooling verify rather than guessing.",
    "Tighten latency on your hottest user interaction before adding a new surface.",
    "Keep secrets out of logs and localStorage; verify security boundaries with a unit test.",
  ]

  static func save(_ text: String) {
    let defaults = UserDefaults.standard
    defaults.set(text, forKey: textKey)
    defaults.set(Date(), forKey: dateKey)
  }

  static func load() -> (text: String, date: Date) {
    let defaults = UserDefaults.standard
    let text = defaults.string(forKey: textKey) ?? "Tap refresh for a concise on-device AI brief."
    let date = (defaults.object(forKey: dateKey) as? Date) ?? Date()
    return (text, date)
  }

  static func nextFallback() -> String {
    let defaults = UserDefaults.standard
    let idx = defaults.integer(forKey: indexKey)
    let next = fallbacks[idx % fallbacks.count]
    defaults.set((idx + 1) % fallbacks.count, forKey: indexKey)
    return next
  }
}

@available(iOS 17.0, *)
enum HanzoBriefService {
  static func generate(prompt: String) async -> String {
    let cleanPrompt = prompt.trimmingCharacters(in: .whitespacesAndNewlines)
    let effectivePrompt = cleanPrompt.isEmpty
      ? "Give one sharp, actionable 15-word engineering or focus insight."
      : cleanPrompt

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
        // Fall through to offline curated brief when Apple Intelligence is unavailable
      }
    }
    #endif

    return HanzoBriefStore.nextFallback()
  }
}

@available(iOS 17.0, *)
struct RefreshHanzoBriefIntent: AppIntent {
  static var title: LocalizedStringResource = "Refresh Hanzo AI Brief"
  static var description = IntentDescription("Generates a fresh concise AI brief directly on your Home Screen.")

  @Parameter(title: "Prompt", default: "Give one sharp, actionable 15-word builder insight.")
  var prompt: String

  init() {}

  init(prompt: String) {
    self.prompt = prompt
  }

  func perform() async throws -> some IntentResult {
    let result = await HanzoBriefService.generate(prompt: prompt)
    HanzoBriefStore.save(result)
    WidgetCenter.shared.reloadTimelines(ofKind: "HanzoBrief")
    return .result()
  }
}

@available(iOS 17.0, *)
struct HanzoBriefConfigurationIntent: WidgetConfigurationIntent {
  static var title: LocalizedStringResource = "Hanzo AI Brief"
  static var description = IntentDescription("Configure the prompt and theme for your Home Screen AI Brief.")

  @Parameter(title: "Prompt", default: "Give one sharp, actionable 15-word builder insight.")
  var prompt: String

  @Parameter(title: "Theme", default: .zen)
  var theme: AskHanzoTheme
}

@available(iOS 17.0, *)
struct HanzoBriefEntry: TimelineEntry {
  let date: Date
  let text: String
  let configuration: HanzoBriefConfigurationIntent
}

@available(iOS 17.0, *)
struct HanzoBriefProvider: AppIntentTimelineProvider {
  func placeholder(in context: Context) -> HanzoBriefEntry {
    HanzoBriefEntry(
      date: Date(),
      text: "Focus on your highest-leverage ship item before checking notifications.",
      configuration: HanzoBriefConfigurationIntent()
    )
  }

  func snapshot(for configuration: HanzoBriefConfigurationIntent, in context: Context) async -> HanzoBriefEntry {
    let stored = HanzoBriefStore.load()
    return HanzoBriefEntry(date: stored.date, text: stored.text, configuration: configuration)
  }

  func timeline(for configuration: HanzoBriefConfigurationIntent, in context: Context) async -> Timeline<HanzoBriefEntry> {
    let stored = HanzoBriefStore.load()
    let entry = HanzoBriefEntry(date: stored.date, text: stored.text, configuration: configuration)
    let nextUpdate = Calendar.current.date(byAdding: .hour, value: 4, to: Date()) ?? Date().addingTimeInterval(14400)
    return Timeline(entries: [entry], policy: .after(nextUpdate))
  }
}

@available(iOS 17.0, *)
struct HanzoBriefEntryView: View {
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
          Text("AI Brief")
            .font(.system(size: 12, weight: .semibold))
            .foregroundStyle(palette.mutedColor)
        }

        Spacer(minLength: 4)

        Button(intent: RefreshHanzoBriefIntent(prompt: entry.configuration.prompt)) {
          Image(systemName: "arrow.clockwise")
            .font(.system(size: 12, weight: .semibold))
            .foregroundStyle(palette.iconColor)
            .frame(width: 28, height: 28)
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
        .font(.system(size: 14, weight: .regular))
        .foregroundStyle(palette.titleColor)
        .lineLimit(4)
        .minimumScaleFactor(0.85)

      Spacer(minLength: 0)
    }
    .padding(2)
    .widgetURL(URL(string: "hanzodev://ask?action=chat")!)
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
    .description("On-device AI brief with one-tap refresh and editable prompt.")
    .supportedFamilies([.systemSmall, .systemMedium])
  }
}
