import AVFoundation
import ExpoModulesCore
import Photos
import Security
import UIKit
import UniformTypeIdentifiers
#if canImport(WidgetKit)
import WidgetKit
#endif

@ExpoModule("HanzoNativeControls")
public final class HanzoNativeControlsModule: Module {
  private let presentationSources = HanzoPresentationSources()
  private var videoPresentation: HanzoNativeVideoPresentation?
  private var filePresentation: HanzoNativeFilePresentation?
  private var imageViewer: HanzoNativeImageViewer?
  /// The last file preview that closed. Written on main, read synchronously from JS.
  private let filePreviewActivity = NSLock()
  private var finishedFilePreview: String?

  public func definition() -> ModuleDefinition {
    Events("videoPlaybackChange")
    Constants {
      if #available(iOS 26.0, *) {
        return [
          "supportsWorkspaceColumns": NSClassFromString("RNSSplitHostComponentView") != nil,
        ]
      }
      return ["supportsWorkspaceColumns": false]
    }
    View(HanzoSegmentedControlView.self) {
      ViewName("SegmentedControl")
      Events("onSelectionChange")
      Prop("labels") { (view: HanzoSegmentedControlView, value: [String]) in view.labels = value }
      Prop("selectedIndex") { (view: HanzoSegmentedControlView, value: Int) in view.selectedIndex = value }
      Prop("trackColor") { (view: HanzoSegmentedControlView, value: UIColor) in view.trackColor = value }
      Prop("selectedColor") { (view: HanzoSegmentedControlView, value: UIColor) in view.selectedColor = value }
      Prop("textColor") { (view: HanzoSegmentedControlView, value: UIColor) in view.textColor = value }
      Prop("mutedColor") { (view: HanzoSegmentedControlView, value: UIColor) in view.mutedColor = value }
      Prop("appearance") { (view: HanzoSegmentedControlView, value: String) in view.appearance = value }
    }

    View(HanzoSidebarTabBarView.self) {
      ViewName("SidebarTabBar")
      Events("onSelectionChange", "onCompose")
      Prop("tabs") { (view: HanzoSidebarTabBarView, value: [String]) in view.tabs = value }
      Prop("selectedTab") { (view: HanzoSidebarTabBarView, value: String) in view.selectedTab = value }
      Prop("selectedColor") { (view: HanzoSidebarTabBarView, value: UIColor) in view.selectedColor = value }
      Prop("showsCompose") { (view: HanzoSidebarTabBarView, value: Bool) in view.showsCompose = value }
      Prop("compact") { (view: HanzoSidebarTabBarView, value: Bool) in view.compact = value }
      Prop("appearance") { (view: HanzoSidebarTabBarView, value: String) in view.appearance = value }
    }

    View(HanzoLayoutMetricsView.self) {
      ViewName("LayoutMetrics")
      Events("onMetricsChange")
    }

    View(HanzoScrollEdgeBleedView.self) {
      ViewName("ScrollEdgeBleed")
    }

    AsyncFunction("generateDpopKey") { HanzoDpopKeys.generate() }

    AsyncFunction("dpopKeyMatches") { (privateKey: Data, publicKey: Data) in
      try HanzoDpopKeys.matches(privateKey: privateKey, publicKey: publicKey)
    }

    AsyncFunction("signDpop") { (privateKey: Data, signingInput: Data) in
      try HanzoDpopKeys.sign(privateKey: privateKey, signingInput: signingInput)
    }

    AsyncFunction("presentVideo") { (url: URL, title: String, sourceIdentifier: String, identifier: String, promise: Promise) in
      try self.presentVideo(
        url: url,
        title: title,
        sourceIdentifier: sourceIdentifier,
        identifier: identifier,
        promise: promise
      )
    }.runOnQueue(.main)

    AsyncFunction("presentFile") { (url: URL, title: String, sourceIdentifier: String, identifier: String, promise: Promise) in
      try self.presentFile(url: url, title: title, sourceIdentifier: sourceIdentifier,
                           identifier: identifier, promise: promise)
    }.runOnQueue(.main)

    // A nil URL is still resolving in JS and arrives later through updateFile.
    AsyncFunction("presentFiles") { (urls: [URL?], titles: [String], selectedIndex: Int, sourceIdentifiers: [String], identifier: String, promise: Promise) in
      guard !urls.isEmpty, urls.count == titles.count, urls.count == sourceIdentifiers.count,
        urls.indices.contains(selectedIndex)
      else {
        throw URLError(.badURL)
      }
      // A viewer still animating closed must not block the next one, as Quick Look's error alert did.
      self.imageViewer?.finishImmediately()
      guard self.filePresentation == nil, self.videoPresentation == nil,
        let presenter = self.appContext?.utilities?.currentViewController()
      else { throw URLError(.cannotLoadFromNetwork) }
      let viewer = HanzoNativeImageViewer(
        identifier: identifier, sources: self.presentationSources, sourceIdentifiers: sourceIdentifiers,
        onFinish: { [weak self] in self?.setFinishedFilePreview(identifier) },
        completion: { [weak self] error in
          if self?.imageViewer?.identifier == identifier { self?.imageViewer = nil }
          if let error { promise.reject(error) } else { promise.resolve(nil) }
        }
      )
      self.imageViewer = viewer
      viewer.present(urls: urls, titles: titles, selectedIndex: selectedIndex, from: presenter)
    }.runOnQueue(.main)

    // A tap can reach JS before the old viewer's close does; this tells it whether that viewer is gone.
    // A presentation main has not started yet reads as still open.
    Function("isFilePreviewFinished") { (identifier: String) -> Bool in
      self.filePreviewActivity.withLock { self.finishedFilePreview == identifier }
    }

    // A nil URL marks the item as one that could not be resolved.
    AsyncFunction("updateFile") { (identifier: String, index: Int, url: URL?) in
      if self.imageViewer?.identifier == identifier { self.imageViewer?.update(index: index, url: url) }
    }.runOnQueue(.main)

    OnDestroy {
      let presentation = self.videoPresentation
      let file = self.filePresentation
      let images = self.imageViewer
      DispatchQueue.main.async {
        presentation?.dismiss()
        file?.dismiss()
        images?.dismiss()
      }
    }

    View(HanzoPresentationSourceView.self) {
      ViewName("PresentationSource")
      Prop("identifier") { (view: HanzoPresentationSourceView, identifier: String) in
        view.sources = self.presentationSources
        view.identifier = identifier
      }
    }

    View(HanzoFrostedCutoutView.self) {
      ViewName("FrostedCutout")
      Prop("cutoutTop") { (view: HanzoFrostedCutoutView, value: Double) in view.cutoutTop = CGFloat(value) }
      Prop("cutoutWidth") { (view: HanzoFrostedCutoutView, value: Double) in view.cutoutWidth = CGFloat(value) }
      Prop("cutoutHeight") { (view: HanzoFrostedCutoutView, value: Double) in view.cutoutHeight = CGFloat(value) }
      Prop("cutoutRadius") { (view: HanzoFrostedCutoutView, value: Double) in view.cutoutRadius = CGFloat(value) }
      Prop("appearance") { (view: HanzoFrostedCutoutView, value: String) in view.appearance = value }
    }

    View(HanzoContextSheetSizeView.self) {
      ViewName("ContextSheetSize")
      Prop("contentHeight") { (view: HanzoContextSheetSizeView, height: Double) in
        view.contentHeight = CGFloat(height)
      }
    }

    AsyncFunction("shareFileFromSource") { (url: URL, title: String, identifier: String, promise: Promise) in
      try self.shareFile(url: url, title: title, sourceIdentifier: identifier, promise: promise)
    }.runOnQueue(.main)

    // Photos names the asset itself, so iOS ignores the Android display name.
    AsyncFunction("saveImageToLibrary") { (url: URL, mimeType: String, name: String, promise: Promise) in
      Self.saveImageToLibrary(url: url, mimeType: mimeType, name: name, promise: promise)
    }

    AsyncFunction("syncWidgetSnapshot") { (payloadJson: String) -> Bool in
      return Self.syncWidgetSnapshot(payloadJson: payloadJson)
    }
  }

  private static let widgetSnapshotKey = "hanzo_widget_snapshot_v1"
  private static let widgetUpdatedAtKey = "hanzo_widget_updated_at"

  private static func resolveWidgetAppGroupId() -> String? {
    guard let bundleId = Bundle.main.bundleIdentifier, !bundleId.isEmpty else {
      return nil
    }
    return "group.\(bundleId)"
  }

  private static func syncWidgetSnapshot(payloadJson: String) -> Bool {
    let now = Date().timeIntervalSince1970
    var wroteShared = false
    if let groupId = resolveWidgetAppGroupId(),
       let sharedDefaults = UserDefaults(suiteName: groupId) {
      sharedDefaults.set(payloadJson, forKey: widgetSnapshotKey)
      sharedDefaults.set(now, forKey: widgetUpdatedAtKey)
      sharedDefaults.synchronize()
      wroteShared = true
    }
    UserDefaults.standard.set(payloadJson, forKey: widgetSnapshotKey)
    UserDefaults.standard.set(now, forKey: widgetUpdatedAtKey)
    #if canImport(WidgetKit)
    if #available(iOS 14.0, *) {
      WidgetCenter.shared.reloadAllTimelines()
    }
    #endif
    return wroteShared
  }

  // UIKit presenters are main-actor isolated rather than run on the JS thread
  // the macro would otherwise pick.
  @JS
  @MainActor
  func dismissVideo(identifier: String) async {
    dismissVideoPresentation(identifier: identifier)
  }

  // expo-video activates the session for inline playback but never deactivates
  // it. Any other category or mode means another feature (recording) owns it.
  @JS
  @MainActor
  func releaseVideoAudioSession() async {
    // The fullscreen presentation releases the session in its own teardown.
    guard videoPresentation == nil else { return }
    let session = AVAudioSession.sharedInstance()
    guard session.category == .playback, session.mode == .moviePlayback else { return }
    try? session.setActive(false, options: [.notifyOthersOnDeactivation])
  }

  @JS
  @MainActor
  func dismissFile(identifier: String) async {
    dismissFilePresentation(identifier: identifier)
  }

  // Rotates the interface without Simulator menu UI scripting, which CI
  // runners cannot perform (osascript is denied Accessibility access there).
  @JS
  @MainActor
  func applyShowcaseOrientation(orientation: String) async {
    let mask: UIInterfaceOrientationMask = orientation == "landscape" ? .landscapeRight : .portrait
    for case let windowScene as UIWindowScene in UIApplication.shared.connectedScenes {
      windowScene.requestGeometryUpdate(.iOS(interfaceOrientations: mask)) { error in
        NSLog("HanzoNativeControls applyShowcaseOrientation(\(orientation)) failed: \(error)")
      }
      for window in windowScene.windows {
        window.rootViewController?.setNeedsUpdateOfSupportedInterfaceOrientations()
      }
    }
  }

  // Geometry requests can fail before foreground activation; callers retry until bounds match.
  // Use screen bounds because iPadOS floating-window orientation can differ from the captured framebuffer.
  @JS
  @MainActor
  func getInterfaceOrientation() async -> String {
    guard
      let windowScene = UIApplication.shared.connectedScenes
        .compactMap({ $0 as? UIWindowScene })
        .first
    else {
      return "unknown"
    }
    let bounds = windowScene.screen.coordinateSpace.bounds
    return bounds.width > bounds.height ? "landscape" : "portrait"
  }

  @JS
  @MainActor
  func isProtectedDataAvailable() async -> Bool {
    UIApplication.shared.isProtectedDataAvailable
  }

  private func launchArgument(_ flag: String) -> String? {
    let arguments = ProcessInfo.processInfo.arguments
    guard
      let flagIndex = arguments.firstIndex(of: flag),
      arguments.indices.contains(flagIndex + 1)
    else {
      return nil
    }
    return arguments[flagIndex + 1]
  }

  @JS
  func getShowcasePairingUrl() -> String? {
    launchArgument("--showcasePairingUrl")
  }

  @JS
  func getShowcaseScene() -> String? {
    let scenePath = NSHomeDirectory() + "/Library/Caches/HanzoShowcaseScene"
    if let storedScene = try? String(contentsOfFile: scenePath, encoding: .utf8)
      .trimmingCharacters(in: .whitespacesAndNewlines), !storedScene.isEmpty {
      return storedScene
    }
    return launchArgument("--showcaseScene")
  }

  // The palette is fixed for the whole capture, so it only ever arrives as a
  // launch argument — unlike the scene, which the runner rewrites in place.
  @JS
  func getShowcaseTheme() -> String? {
    launchArgument("--showcaseTheme")
  }

  @JS
  func getShowcaseOrientation() -> String? {
    launchArgument("--showcaseOrientation")
  }

  @JS
  func prepareShowcaseCapture() {
    for itemClass in [kSecClassGenericPassword, kSecClassInternetPassword] {
      SecItemDelete([kSecClass as String: itemClass] as CFDictionary)
    }
  }

  @JS(.concurrent)
  func markShowcaseReady(scene: String) async {
    let readyPath = NSHomeDirectory() + "/Library/Caches/HanzoShowcaseReadyScene"
    try? scene.write(toFile: readyPath, atomically: true, encoding: .utf8)
  }

  private func presentVideo(url: URL, title: String, sourceIdentifier: String, identifier: String, promise: Promise) throws {
    let isPlayableURL = url.isFileURL
      ? FileManager.default.isReadableFile(atPath: url.path)
      : (["https", "http"].contains(url.scheme?.lowercased() ?? "") && url.host != nil)
    guard videoPresentation == nil, filePresentation == nil, imageViewer == nil,
      let presenter = appContext?.utilities?.currentViewController(),
      isPlayableURL
    else {
      throw NSError(
        domain: "HanzoNativeVideo",
        code: 2,
        userInfo: [NSLocalizedDescriptionKey: "The video preview is no longer available."]
      )
    }
    let presentation = HanzoNativeVideoPresentation(
      identifier: identifier, url: url, title: title,
      onPlaybackChange: { [weak self] isPlaying in
        self?.sendEvent("videoPlaybackChange", ["identifier": identifier, "isPlaying": isPlaying])
      },
      completion: { [weak self] error in
        self?.videoPresentation = nil
        if let error { promise.reject(error) } else { promise.resolve(nil) }
      }
    )
    videoPresentation = presentation
    presentation.present(from: presenter, sources: presentationSources, sourceIdentifier: sourceIdentifier)
  }

  private func dismissVideoPresentation(identifier: String) {
    if videoPresentation?.identifier == identifier { videoPresentation?.dismiss() }
  }

  private func makeFilePresentation(sourceIdentifier: String, identifier: String,
                                    promise: Promise) throws -> HanzoNativeFilePresentation {
    guard filePresentation == nil, videoPresentation == nil, imageViewer == nil,
      appContext?.utilities?.currentViewController() != nil
    else { throw URLError(.cannotLoadFromNetwork) }
    let file = HanzoNativeFilePresentation(
      identifier: identifier, sources: presentationSources, sourceIdentifier: sourceIdentifier,
      onFinish: { [weak self] in self?.setFinishedFilePreview(identifier) },
      completion: { [weak self] error in
        self?.filePresentation = nil
        if let error { promise.reject(error) } else { promise.resolve(nil) }
      }
    )
    filePresentation = file
    return file
  }

  private func setFinishedFilePreview(_ identifier: String) {
    filePreviewActivity.withLock { finishedFilePreview = identifier }
  }

  private func presentFile(url: URL, title: String, sourceIdentifier: String,
                           identifier: String, promise: Promise) throws {
    let file = try makeFilePresentation(sourceIdentifier: sourceIdentifier, identifier: identifier, promise: promise)
    guard let presenter = appContext?.utilities?.currentViewController() else { file.dismiss(); return }
    file.present(url: url, title: title, from: presenter)
  }

  private func dismissFilePresentation(identifier: String) {
    if filePresentation?.identifier == identifier { filePresentation?.dismiss() }
    if imageViewer?.identifier == identifier { imageViewer?.dismiss() }
  }

  private static func saveImageToLibrary(url: URL, mimeType: String, name: String, promise: Promise) {
    PHPhotoLibrary.requestAuthorization(for: .addOnly) { status in
      guard status == .authorized || status == .limited else {
        promise.reject(NSError(
          domain: "HanzoNativePhotos",
          code: 1,
          userInfo: [NSLocalizedDescriptionKey: "Allow Hanzo Dev to add photos in Settings."]
        ))
        return
      }
      PHPhotoLibrary.shared().performChanges({
        let options = PHAssetResourceCreationOptions()
        options.uniformTypeIdentifier = UTType(mimeType: mimeType)?.identifier
        options.originalFilename = name
        PHAssetCreationRequest.forAsset().addResource(with: .photo, fileURL: url, options: options)
      }, completionHandler: { saved, error in
        if saved {
          promise.resolve(nil)
        } else {
          promise.reject(error ?? NSError(
            domain: "HanzoNativePhotos",
            code: 2,
            userInfo: [NSLocalizedDescriptionKey: "Photos could not save this image."]
          ))
        }
      })
    }
  }

  private func shareFile(url: URL, title: String, sourceIdentifier: String, promise: Promise) throws {
    guard let presenter = appContext?.utilities?.currentViewController() else {
      throw NSError(
        domain: "HanzoNativePresentation",
        code: 2,
        userInfo: [NSLocalizedDescriptionKey: "The presenting screen is no longer open."]
      )
    }
    try presentFileShare(
      url: url,
      title: title,
      source: presentationSources.view(for: sourceIdentifier),
      presenter: presenter,
      promise: promise
    )
  }
}
