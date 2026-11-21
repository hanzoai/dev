import Foundation

struct UsageWindow: Decodable, Hashable {
  let kind: String
  let label: String
  let usedPercent: Double
  let resetsAt: String?
}

struct Credits: Decodable, Hashable {
  let currency: String
  let balance: Double?
  let spent: Double?
  let limit: Double?
}

struct ProviderStatus: Decodable, Hashable {
  let indicator: String
  let description: String?
}

/// One reading of one account on one provider, as served by `GET /v1/usage`.
struct Snapshot: Decodable, Identifiable, Hashable {
  let provider: String
  let fetchedAt: String
  let plan: String?
  let account: String?
  let windows: [UsageWindow]
  let credits: Credits?
  let status: ProviderStatus?
  let error: String?

  var id: String { provider + "/" + (account ?? "") }
  var isHanzo: Bool { provider == "hanzo" }

  var title: String {
    let name = providerName(provider)
    guard let account, !account.isEmpty else { return name }
    return "\(name) · \(account)"
  }

  /// The most constrained window: what the tile bar and the status reading show.
  var primary: UsageWindow? { windows.max { $0.usedPercent < $1.usedPercent } }
}

struct Summary: Decodable {
  let generatedAt: String
  let snapshots: [Snapshot]
}

struct Alert: Decodable {
  let title: String
  let body: String
}

struct TrayPayload: Decodable {
  struct Tray: Decodable { let title: String }
  let tray: Tray
  let alerts: [Alert]
}

private let names = [
  "hanzo": "Hanzo", "claude": "Claude", "codex": "Codex", "copilot": "Copilot",
  "cursor": "Cursor", "opencode": "OpenCode", "antigravity": "Antigravity",
  "zai": "z.ai", "gemini": "Gemini", "kiro": "Kiro", "grok": "Grok",
]

func providerName(_ id: String) -> String { names[id] ?? id.prefix(1).uppercased() + id.dropFirst() }

func parseInstant(_ s: String) -> Date? {
  if let d = try? Date.ISO8601FormatStyle(includingFractionalSeconds: true).parse(s) { return d }
  return try? Date.ISO8601FormatStyle().parse(s)
}

/// "3h 53m", "3d 20h", "12m".
func countdown(to date: Date, from now: Date = Date()) -> String {
  let s = max(0, Int(date.timeIntervalSince(now)))
  let (d, h, m) = (s / 86_400, (s % 86_400) / 3_600, (s % 3_600) / 60)
  if d > 0 { return "\(d)d \(h)h" }
  if h > 0 { return "\(h)h \(m)m" }
  return "\(max(m, 1))m"
}

func ago(_ date: Date, now: Date = Date()) -> String {
  let s = Int(now.timeIntervalSince(date))
  if s < 45 { return "just now" }
  if s < 3_600 { return "\(s / 60)m ago" }
  if s < 86_400 { return "\(s / 3_600)h ago" }
  return "\(s / 86_400)d ago"
}

func money(_ v: Double, _ currency: String) -> String {
  let f = NumberFormatter()
  f.numberStyle = .currency
  f.currencyCode = currency.uppercased()
  return f.string(from: NSNumber(value: v)) ?? String(format: "%.2f %@", v, currency)
}
