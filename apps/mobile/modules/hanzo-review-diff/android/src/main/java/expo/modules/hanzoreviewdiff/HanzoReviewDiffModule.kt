package expo.modules.hanzoreviewdiff

import expo.modules.kotlin.Promise
import expo.modules.kotlin.modules.Module
import expo.modules.kotlin.modules.ModuleDefinition

private val REVIEW_DIFF_EVENTS = arrayOf(
  "onDebug",
  "onVisibleFileChange",
  "onToggleFile",
  "onToggleViewedFile",
  "onPressLine",
  "onToggleComment",
  "onPressNotice",
)

class HanzoReviewDiffModule : Module() {
  override fun definition() = ModuleDefinition {
    Name("HanzoReviewDiffSurface")

    View(HanzoReviewDiffView::class) {
      Prop("tokensResetKey") { view: HanzoReviewDiffView, tokensResetKey: String ->
        view.setTokensResetKey(tokensResetKey)
      }
      Prop("contentResetKey") { view: HanzoReviewDiffView, contentResetKey: String ->
        view.setContentResetKey(contentResetKey)
      }
      Prop("collapsedFileIdsJson") { view: HanzoReviewDiffView, collapsedFileIdsJson: String ->
        view.setCollapsedFileIdsJson(collapsedFileIdsJson)
      }
      Prop("viewedFileIdsJson") { view: HanzoReviewDiffView, viewedFileIdsJson: String ->
        view.setViewedFileIdsJson(viewedFileIdsJson)
      }
      Prop("selectedRowIdsJson") { view: HanzoReviewDiffView, selectedRowIdsJson: String ->
        view.setSelectedRowIdsJson(selectedRowIdsJson)
      }
      Prop("collapsedCommentIdsJson") { view: HanzoReviewDiffView, value: String ->
        view.setCollapsedCommentIdsJson(value)
      }
      Prop("appearanceScheme") { view: HanzoReviewDiffView, value: String ->
        view.setAppearanceScheme(value)
      }
      Prop("themeJson") { view: HanzoReviewDiffView, value: String -> view.setThemeJson(value) }
      Prop("styleJson") { view: HanzoReviewDiffView, value: String -> view.setStyleJson(value) }
      Prop("rowHeight") { view: HanzoReviewDiffView, value: Double ->
        view.setRowHeight(value.toFloat())
      }
      // Accept the shared prop while deriving Android's scroll range from the drawn text.
      Prop("contentWidth") { _: HanzoReviewDiffView, _: Double -> }
      Prop("contentInsetBottom") { view: HanzoReviewDiffView, value: Double ->
        view.setContentInsetBottom(value.toFloat())
      }
      Prop("scrollEnabled") { view: HanzoReviewDiffView, enabled: Boolean ->
        view.setScrollEnabled(enabled)
      }
      Prop("initialRowIndex") { view: HanzoReviewDiffView, initialRowIndex: Double ->
        view.setInitialRowIndex(initialRowIndex)
      }
      Prop("initialFileId") { view: HanzoReviewDiffView, fileId: String ->
        view.setInitialFileId(fileId)
      }

      Events(REVIEW_DIFF_EVENTS)

      AsyncFunction("scrollToFile") {
          view: HanzoReviewDiffView,
          fileId: String,
          animated: Boolean
        ->
        view.scrollToFile(fileId, animated)
      }
      AsyncFunction("scrollToTop") { view: HanzoReviewDiffView, animated: Boolean ->
        view.scrollToTop(animated)
      }
      AsyncFunction("setRowsJson") {
          view: HanzoReviewDiffView,
          rowsJson: String,
          resetKey: String
        ->
        view.setContentResetKey(resetKey)
        view.setRowsJson(rowsJson)
      }
      AsyncFunction("setTokensJson") { view: HanzoReviewDiffView, tokensJson: String ->
        view.setTokensJson(tokensJson)
      }
      AsyncFunction("setTokensPatchJson") {
          view: HanzoReviewDiffView,
          tokensPatchJson: String,
          promise: Promise
        ->
        view.setTokensPatchJson(tokensPatchJson) { accepted -> promise.resolve(accepted) }
      }

      OnViewDestroys { view: HanzoReviewDiffView ->
        view.cleanup()
      }
    }
  }
}
