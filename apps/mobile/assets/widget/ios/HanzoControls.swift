import AppIntents
import SwiftUI
import WidgetKit

// MARK: - iOS 18+ Control Center, Lock Screen & Action Button Controls

#if swift(>=6.0)
@available(iOS 18.0, *)
struct HanzoLauncherControlConfig: ControlConfigurationIntent {
  static var title: LocalizedStringResource = "Hanzo Control"
  static var description = IntentDescription(
    "Choose which Hanzo AI action this Control Center button or Action Button launches."
  )

  @Parameter(title: "Action", default: .chat)
  var action: HanzoQuickAction
}

@available(iOS 18.0, *)
struct HanzoLauncherControlProvider: AppIntentControlValueProvider {
  func previewValue(configuration: HanzoLauncherControlConfig) -> HanzoQuickAction {
    configuration.action
  }

  func currentValue(configuration: HanzoLauncherControlConfig) async throws -> HanzoQuickAction {
    configuration.action
  }
}

@available(iOS 18.0, *)
struct AskHanzoControl: ControlWidget {
  static let kind: String = "ai.hanzo.control.ask"

  var body: some ControlWidgetConfiguration {
    StaticControlConfiguration(kind: Self.kind) {
      ControlWidgetButton(action: OpenHanzoActionIntent(action: .chat)) {
        Label("Ask Hanzo", systemImage: "sparkles")
      }
    }
    .displayName("Ask Hanzo")
    .description("Launch Hanzo AI chat from Control Center, Lock Screen, or Action Button.")
  }
}

@available(iOS 18.0, *)
struct HanzoVoiceControl: ControlWidget {
  static let kind: String = "ai.hanzo.control.voice"

  var body: some ControlWidgetConfiguration {
    StaticControlConfiguration(kind: Self.kind) {
      ControlWidgetButton(action: OpenHanzoActionIntent(action: .voice)) {
        Label("Hanzo Voice", systemImage: "mic.fill")
      }
    }
    .displayName("Hanzo Voice")
    .description("Start voice chat with Hanzo AI instantly.")
  }
}

@available(iOS 18.0, *)
struct HanzoVisionControl: ControlWidget {
  static let kind: String = "ai.hanzo.control.vision"

  var body: some ControlWidgetConfiguration {
    StaticControlConfiguration(kind: Self.kind) {
      ControlWidgetButton(action: OpenHanzoActionIntent(action: .camera)) {
        Label("Hanzo Vision", systemImage: "camera.viewfinder")
      }
    }
    .displayName("Hanzo Vision")
    .description("Open Hanzo camera and visual AI analysis.")
  }
}

@available(iOS 18.0, *)
struct HanzoDevControl: ControlWidget {
  static let kind: String = "ai.hanzo.control.dev"

  var body: some ControlWidgetConfiguration {
    StaticControlConfiguration(kind: Self.kind) {
      ControlWidgetButton(action: OpenHanzoActionIntent(action: .dev)) {
        Label("Hanzo Dev", systemImage: "terminal.fill")
      }
    }
    .displayName("Hanzo Dev")
    .description("Jump straight into a new Hanzo Dev agent task.")
  }
}

@available(iOS 18.0, *)
struct HanzoLauncherControl: ControlWidget {
  static let kind: String = "ai.hanzo.control.launcher"

  var body: some ControlWidgetConfiguration {
    AppIntentControlConfiguration(
      kind: Self.kind,
      provider: HanzoLauncherControlProvider()
    ) { action in
      ControlWidgetButton(action: OpenHanzoActionIntent(action: action)) {
        Label(action.label, systemImage: action.symbol)
      }
    }
    .displayName("Hanzo Quick Control")
    .description("Configurable Control Center & Action Button shortcut for any Hanzo AI action.")
  }
}
#endif
