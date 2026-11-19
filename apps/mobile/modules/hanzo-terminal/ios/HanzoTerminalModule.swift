import ExpoModulesCore

public class HanzoTerminalModule: Module {
  public func definition() -> ModuleDefinition {
    Name("HanzoTerminalSurface")

    // Bumped when native hardware-keyboard handling changes; surfaced in the JS debug
    // logs so a stale native binary is distinguishable from a broken key pipeline.
    Constants([
      "hardwareKeyRevision": 4,
      "outputRevision": 1,
    ])

    View(HanzoTerminalView.self) {
      AsyncFunction("writeOutput") { (view: HanzoTerminalView, data: String, reset: Bool) in
        view.writeOutput(data, reset: reset)
      }

      AsyncFunction("resetInput") { (view: HanzoTerminalView) in
        view.resetInputField()
      }

      Prop("terminalKey") { (view: HanzoTerminalView, terminalKey: String) in
        view.terminalKey = terminalKey
      }

      Prop("initialBuffer") { (view: HanzoTerminalView, initialBuffer: String) in
        view.initialBuffer = initialBuffer
      }

      Prop("fontSize") { (view: HanzoTerminalView, fontSize: Double) in
        view.fontSize = CGFloat(fontSize)
      }

      Prop("focusRequest") { (view: HanzoTerminalView, focusRequest: Double) in
        view.focusRequest = focusRequest
      }

      Prop("readOnly") { (view: HanzoTerminalView, readOnly: Bool) in
        view.readOnly = readOnly
      }

      Prop("autoFocus") { (view: HanzoTerminalView, autoFocus: Bool) in
        view.autoFocus = autoFocus
      }

      Prop("appearanceScheme") { (view: HanzoTerminalView, appearanceScheme: String) in
        view.appearanceScheme = appearanceScheme
      }

      Prop("themeConfig") { (view: HanzoTerminalView, themeConfig: String) in
        view.themeConfig = themeConfig
      }

      Prop("backgroundColor") { (view: HanzoTerminalView, backgroundColor: String) in
        view.backgroundColorHex = backgroundColor
      }

      Prop("foregroundColor") { (view: HanzoTerminalView, foregroundColor: String) in
        view.foregroundColorHex = foregroundColor
      }

      Prop("mutedForegroundColor") { (view: HanzoTerminalView, mutedForegroundColor: String) in
        view.mutedForegroundColorHex = mutedForegroundColor
      }

      Prop("captureRequest") { (view: HanzoTerminalView, request: Double) in
        view.captureRequest = request
      }

      Prop("sharedGrid") { (view: HanzoTerminalView, sharedGrid: [Int]) in
        view.sharedGrid = sharedGrid
      }
      Events("onInput", "onResize", "onCapture", "onPaste")

      OnViewDidUpdateProps { (view: HanzoTerminalView) in
        view.applyPendingBuffer()
      }
    }
  }
}
