import ExpoModulesCore
import Foundation

public final class HanzoPreviewTunnelModule: Module {
  public func definition() -> ModuleDefinition {
    Name("HanzoPreviewTunnel")

    Function("resolveAccess") { (requestId: String, url: String?) in
      HanzoTunnelAccess.shared.resolve(requestId: requestId, url: url)
    }

    View(HanzoTunnelWebView.self) {
      Events("onAccessRequest", "onNavigationChange", "onLoadError", "onPageScroll")

      Prop("environmentId") { (view: HanzoTunnelWebView, environmentId: String) in
        view.setEnvironment(environmentId)
      }

      Prop("topInset") { (view: HanzoTunnelWebView, inset: Double) in
        view.setTopInset(CGFloat(inset))
      }

      Prop("url") { (view: HanzoTunnelWebView, url: String) in
        view.load(url)
      }

      AsyncFunction("loadUrl") { (view: HanzoTunnelWebView, url: String) in
        view.navigate(url)
      }.runOnQueue(.main)

      AsyncFunction("goBack") { (view: HanzoTunnelWebView) in
        view.goBack()
      }.runOnQueue(.main)

      AsyncFunction("goForward") { (view: HanzoTunnelWebView) in
        view.goForward()
      }.runOnQueue(.main)

      AsyncFunction("reload") { (view: HanzoTunnelWebView) in
        view.reload()
      }.runOnQueue(.main)
    }
  }
}

/// Pending asks for a tunnel URL. JavaScript owns the environment's credentials and the route in
/// use, so every connection asks it for a freshly ticketed URL.
final class HanzoTunnelAccess {
  static let shared = HanzoTunnelAccess()

  private let lock = NSLock()
  private var pending: [String: (String?) -> Void] = [:]
  private var nextId = 0

  func request(ask: (String) -> Void, completion: @escaping (String?) -> Void) {
    let requestId: String = lock.withLock {
      nextId += 1
      let requestId = "preview-tunnel-\(nextId)"
      pending[requestId] = completion
      return requestId
    }
    DispatchQueue.global().asyncAfter(deadline: .now() + 15) { [weak self] in
      self?.resolve(requestId: requestId, url: nil)
    }
    ask(requestId)
  }

  func resolve(requestId: String, url: String?) {
    let completion = lock.withLock { pending.removeValue(forKey: requestId) }
    completion?(url)
  }
}
