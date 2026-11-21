import Foundation

struct Endpoint {
  let base: URL
  let token: String

  /// The Dev server this helper belongs to: set by the app that launches it.
  static func fromEnvironment() -> Endpoint? {
    let env = ProcessInfo.processInfo.environment
    guard let raw = env["HANZO_DEV_URL"], let base = URL(string: raw), let token = env["HANZO_DEV_TOKEN"]
    else { return nil }
    return Endpoint(base: base, token: token)
  }
}

struct Client {
  let endpoint: Endpoint

  private func send(_ method: String, _ path: String, body: Data? = nil) async throws -> Data {
    var req = URLRequest(url: endpoint.base.appendingPathComponent(path))
    req.httpMethod = method
    req.timeoutInterval = 30
    req.setValue("Bearer \(endpoint.token)", forHTTPHeaderField: "Authorization")
    if let body {
      req.httpBody = body
      req.setValue("application/json", forHTTPHeaderField: "Content-Type")
    }
    let (data, response) = try await URLSession.shared.data(for: req)
    guard let http = response as? HTTPURLResponse, (200..<300).contains(http.statusCode) else {
      throw URLError(.badServerResponse)
    }
    return data
  }

  func summary() async throws -> Summary {
    try JSONDecoder().decode(Summary.self, from: await send("GET", "v1/usage"))
  }

  func refresh() async throws -> Summary {
    try JSONDecoder().decode(Summary.self, from: await send("POST", "v1/usage/refresh"))
  }

  /// The menu-bar reading and the threshold alerts. Reading drains the alerts.
  func tray() async throws -> TrayPayload {
    try JSONDecoder().decode(TrayPayload.self, from: await send("GET", "v1/usage/tray"))
  }

  func showInMenuBar() async throws -> Bool {
    let object = try JSONSerialization.jsonObject(with: await send("GET", "v1/usage/settings"))
    return (object as? [String: Any])?["showInMenuBar"] as? Bool ?? false
  }

  func setShowInMenuBar(_ on: Bool) async throws {
    let raw = try await send("GET", "v1/usage/settings")
    guard var settings = try JSONSerialization.jsonObject(with: raw) as? [String: Any] else {
      throw URLError(.cannotParseResponse)
    }
    settings["showInMenuBar"] = on
    _ = try await send("PUT", "v1/usage/settings", body: JSONSerialization.data(withJSONObject: settings))
  }
}
