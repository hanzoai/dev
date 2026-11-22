import AppIntents
import Foundation
#if canImport(WidgetKit)
import WidgetKit
#endif
#if canImport(FoundationModels)
import FoundationModels
#endif

// MARK: - Shared Action Enum (used by Widgets, Control Center, Action Button & Shortcuts)

@available(iOS 17.0, *)
enum HanzoQuickAction: String, CaseIterable, AppEnum {
  case chat
  case camera
  case voice
  case attach
  case dev
  case clipboard
  case models
  case usage
  case environments
  case permissions
  case settings

  static var typeDisplayRepresentation = TypeDisplayRepresentation(name: "Hanzo Action")

  static var caseDisplayRepresentations: [HanzoQuickAction: DisplayRepresentation] = [
    .chat: DisplayRepresentation(title: "Ask Hanzo", image: .init(systemName: "bubble.left")),
    .camera: DisplayRepresentation(title: "Hanzo Vision", image: .init(systemName: "camera")),
    .voice: DisplayRepresentation(title: "Hanzo Voice", image: .init(systemName: "mic")),
    .attach: DisplayRepresentation(title: "Attach File", image: .init(systemName: "paperclip")),
    .dev: DisplayRepresentation(title: "Hanzo Dev", image: .init(systemName: "terminal")),
    .clipboard: DisplayRepresentation(title: "Ask Clipboard", image: .init(systemName: "doc.on.clipboard")),
    .models: DisplayRepresentation(title: "Models", image: .init(systemName: "sparkles")),
    .usage: DisplayRepresentation(title: "Usage", image: .init(systemName: "chart.bar")),
    .environments: DisplayRepresentation(title: "Machines", image: .init(systemName: "desktopcomputer")),
    .permissions: DisplayRepresentation(title: "AI Permissions", image: .init(systemName: "hand.raised")),
    .settings: DisplayRepresentation(title: "Settings", image: .init(systemName: "gearshape")),
  ]

  var symbol: String {
    switch self {
    case .chat: return "bubble.left"
    case .camera: return "camera"
    case .voice: return "mic"
    case .attach: return "paperclip"
    case .dev: return "terminal"
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
    case .chat: return "Ask Hanzo"
    case .camera: return "Vision"
    case .voice: return "Voice"
    case .attach: return "Attach"
    case .dev: return "Dev"
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
    case .camera: raw = "hanzo://ask?action=camera"
    case .voice: raw = "hanzo://chat/voice"
    case .attach: raw = "hanzo://ask?action=attach"
    case .dev: raw = "hanzo://new"
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

// MARK: - Lightweight Native AI Service (On-Device Apple FoundationModels -> Hanzo Cloud fallback)

@available(iOS 17.0, *)
enum HanzoNativeAIService {
  static func complete(prompt: String, context: String? = nil) async -> String {
    let trimmedPrompt = prompt.trimmingCharacters(in: .whitespacesAndNewlines)
    guard !trimmedPrompt.isEmpty else {
      return "Ask Hanzo anything."
    }

    let fullPrompt: String
    if let ctx = context?.trimmingCharacters(in: .whitespacesAndNewlines), !ctx.isEmpty {
      fullPrompt = "\(trimmedPrompt)\n\nContext:\n\(ctx)"
    } else {
      fullPrompt = trimmedPrompt
    }

    // 1. On-device Apple Foundation Models (iOS 26+, zero latency, private)
    #if canImport(FoundationModels)
    if #available(iOS 26.0, *) {
      do {
        let session = LanguageModelSession()
        let response = try await session.respond(to: fullPrompt)
        let text = response.content.trimmingCharacters(in: .whitespacesAndNewlines)
        if !text.isEmpty {
          return text
        }
      } catch {
        // Fall through to Hanzo Cloud when Apple Intelligence is disabled or unsupported
      }
    }
    #endif

    // 2. Hanzo Cloud public fast lane (short timeout safe for Siri/Shortcuts/Widget execution)
    guard let url = URL(string: "https://api.hanzo.ai/v1/chat/public") else {
      return "Open Hanzo to continue."
    }
    var request = URLRequest(url: url)
    request.httpMethod = "POST"
    request.timeoutInterval = 12
    request.setValue("application/json", forHTTPHeaderField: "Content-Type")

    let payload: [String: Any] = [
      "model": "enso-flash",
      "messages": [
        ["role": "system", "content": "You are Hanzo AI on iOS. Answer concisely and clearly."],
        ["role": "user", "content": fullPrompt],
      ],
      "max_tokens": 300,
      "stream": false,
    ]
    request.httpBody = try? JSONSerialization.data(withJSONObject: payload)

    do {
      let (data, response) = try await URLSession.shared.data(for: request)
      if let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode),
         let json = try JSONSerialization.jsonObject(with: data) as? [String: Any],
         let choices = json["choices"] as? [[String: Any]],
         let message = choices.first?["message"] as? [String: Any],
         let content = message["content"] as? String {
        let clean = content.trimmingCharacters(in: .whitespacesAndNewlines)
        if !clean.isEmpty {
          return clean
        }
      }
    } catch {
      // Offline fallback
    }

    return "Could not reach Hanzo AI right now. Open Hanzo to continue."
  }
}

// MARK: - App Intents for Siri, Spotlight, Shortcuts, Control Center & Action Button

@available(iOS 17.0, *)
struct AskHanzoIntent: AppIntent {
  static var title: LocalizedStringResource = "Ask Hanzo"
  static var description = IntentDescription(
    "Ask Hanzo AI a question from Siri, Shortcuts, or Spotlight and return the answer."
  )
  static var openAppWhenRun: Bool = false

  @Parameter(title: "Question")
  var prompt: String

  @Parameter(title: "Context")
  var context: String?

  static var parameterSummary: some ParameterSummary {
    Summary("Ask Hanzo \(\.$prompt)") {
      \.$context
    }
  }

  func perform() async throws -> some IntentResult & ReturnsValue<String> & ProvidesDialog {
    let answer = await HanzoNativeAIService.complete(prompt: prompt, context: context)
    return .result(value: answer, dialog: IntentDialog(stringLiteral: answer))
  }
}

@available(iOS 17.0, *)
struct RunHanzoAutomationIntent: AppIntent {
  static var title: LocalizedStringResource = "Run Hanzo AI Automation"
  static var description = IntentDescription(
    "Transform text, files, calendar items, or clipboard content with Hanzo AI inside Apple Shortcuts."
  )
  static var openAppWhenRun: Bool = false

  @Parameter(title: "Instruction")
  var instruction: String

  @Parameter(title: "Input Data")
  var inputText: String?

  static var parameterSummary: some ParameterSummary {
    Summary("Run Hanzo \(\.$instruction) on \(\.$inputText)")
  }

  func perform() async throws -> some IntentResult & ReturnsValue<String> {
    let output = await HanzoNativeAIService.complete(prompt: instruction, context: inputText)
    return .result(value: output)
  }
}

@available(iOS 17.0, *)
struct RefreshHanzoWidgetIntent: AppIntent {
  static var title: LocalizedStringResource = "Refresh Hanzo Widget"
  static var description = IntentDescription(
    "Reloads Hanzo Home Screen widgets with the latest synced chats and environments."
  )
  static var openAppWhenRun: Bool = false

  init() {}

  func perform() async throws -> some IntentResult {
    #if canImport(WidgetKit)
    WidgetCenter.shared.reloadAllTimelines()
    #endif
    return .result()
  }
}

@available(iOS 18.0, *)
struct OpenHanzoActionIntent: AppIntent {
  static var title: LocalizedStringResource = "Open Hanzo Action"
  static var description = IntentDescription(
    "Launch Hanzo directly into Chat, Voice, Vision, Files, Dev Agent, or Permissions."
  )
  static var openAppWhenRun: Bool = true

  @Parameter(title: "Action", default: .chat)
  var action: HanzoQuickAction

  init() {
    self.action = .chat
  }

  init(action: HanzoQuickAction) {
    self.action = action
  }

  func perform() async throws -> some IntentResult & OpensIntent {
    return .result(opensIntent: OpenURLIntent(action.url))
  }
}

@available(iOS 18.0, *)
struct HanzoAppShortcutsProvider: AppShortcutsProvider {
  static var appShortcuts: [AppShortcut] {
    AppShortcut(
      intent: AskHanzoIntent(),
      phrases: [
        "Ask \(.applicationName)",
        "Ask \(.applicationName) a question",
      ],
      shortTitle: "Ask Hanzo",
      systemImageName: "sparkles"
    )
    AppShortcut(
      intent: OpenHanzoActionIntent(action: .voice),
      phrases: [
        "Talk to \(.applicationName)",
        "Voice chat with \(.applicationName)",
      ],
      shortTitle: "Hanzo Voice",
      systemImageName: "mic"
    )
    AppShortcut(
      intent: OpenHanzoActionIntent(action: .camera),
      phrases: [
        "Open \(.applicationName) Vision",
        "Scan with \(.applicationName)",
      ],
      shortTitle: "Hanzo Vision",
      systemImageName: "camera"
    )
    AppShortcut(
      intent: OpenHanzoActionIntent(action: .dev),
      phrases: [
        "Open \(.applicationName) Dev",
        "Start coding in \(.applicationName)",
      ],
      shortTitle: "Hanzo Dev",
      systemImageName: "terminal"
    )
    AppShortcut(
      intent: RunHanzoAutomationIntent(),
      phrases: [
        "Run \(.applicationName) automation",
      ],
      shortTitle: "Hanzo Automation",
      systemImageName: "wand.and.stars"
    )
  }
}
