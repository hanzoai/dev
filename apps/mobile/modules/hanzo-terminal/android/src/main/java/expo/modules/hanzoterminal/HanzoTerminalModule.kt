package expo.modules.hanzoterminal

import expo.modules.kotlin.modules.Module
import expo.modules.kotlin.modules.ModuleDefinition

class HanzoTerminalModule : Module() {
  override fun definition() = ModuleDefinition {
    Name("HanzoTerminalSurface")

    // Bumped when native hardware-keyboard handling changes; surfaced in the JS debug
    // logs so a stale native binary is distinguishable from a broken key pipeline.
    Constants(
      "hardwareKeyRevision" to 2,
      "outputRevision" to 1,
    )

    View(HanzoTerminalView::class) {
      AsyncFunction("writeOutput") { view: HanzoTerminalView, data: String, reset: Boolean ->
        view.writeOutput(data, reset)
      }

      // The Android input field already clears after every change.
      AsyncFunction("resetInput") { _: HanzoTerminalView -> }

      Prop("terminalKey") { view: HanzoTerminalView, terminalKey: String ->
        view.terminalKey = terminalKey
      }

      Prop("initialBuffer") { view: HanzoTerminalView, initialBuffer: String ->
        view.initialBuffer = initialBuffer
      }

      Prop("fontSize") { view: HanzoTerminalView, fontSize: Double ->
        view.fontSize = fontSize.toFloat()
      }

      Prop("focusRequest") { view: HanzoTerminalView, focusRequest: Double ->
        view.focusRequest = focusRequest
      }

      Prop("readOnly") { view: HanzoTerminalView, readOnly: Boolean ->
        view.readOnly = readOnly
      }

      Prop("autoFocus") { view: HanzoTerminalView, autoFocus: Boolean ->
        view.autoFocus = autoFocus
      }

      Prop("appearanceScheme") { view: HanzoTerminalView, appearanceScheme: String ->
        view.appearanceScheme = appearanceScheme
      }

      Prop("themeConfig") { view: HanzoTerminalView, themeConfig: String ->
        view.themeConfig = themeConfig
      }

      Prop("backgroundColor") { view: HanzoTerminalView, backgroundColor: String ->
        view.backgroundColorHex = backgroundColor
      }

      Prop("foregroundColor") { view: HanzoTerminalView, foregroundColor: String ->
        view.foregroundColorHex = foregroundColor
      }

      Prop("mutedForegroundColor") { view: HanzoTerminalView, mutedForegroundColor: String ->
        view.mutedForegroundColorHex = mutedForegroundColor
      }

      Prop("captureRequest") { view: HanzoTerminalView, request: Double ->
        view.captureRequest = request
      }

      Prop("sharedGrid") { view: HanzoTerminalView, sharedGrid: List<Int> ->
        view.sharedGrid = sharedGrid
      }
      Events("onInput", "onResize", "onCapture")

      OnViewDidUpdateProps { view: HanzoTerminalView ->
        view.applyPendingBuffer()
      }

      OnViewDestroys { view: HanzoTerminalView ->
        view.cleanup()
      }
    }
  }
}
