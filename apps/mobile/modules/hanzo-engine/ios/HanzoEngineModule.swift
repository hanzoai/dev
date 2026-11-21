import CryptoKit
import ExpoModulesCore
import Foundation
#if canImport(HanzoEngineFFI)
import HanzoEngineFFI
#endif

/// A Zen model this phone can run (0.5 GB, 0.9 GiB resident; a 3B Q4_K_M needs 3.7 GiB and was not coherent on the simulator), from Hanzo's model host (the `zenlm` org). The file is a GGUF the
/// engine loads on Metal; `sha256` is the host's published digest and a download must match it.
private struct Entry {
  let id: String
  let name: String
  let repo: String
  let file: String
  let bytes: Int64
  let sha256: String
  /// The least physical memory, in GiB, the phone should have.
  let memory: Int
}

private let models: [Entry] = [
  Entry(
    id: "zen6-nano-2b", name: "Zen 6 Nano 2B", repo: "zenlm/zen6-nano-2b",
    file: "zen6-nano-2b-PQ2_0.gguf", bytes: 517_313_216,
    sha256: "b248a5db9793e886a5e4eb9cc59d0dbd7494dc2835462a98e69ead6c28ffbb4c", memory: 4)
]

private let host = "https://huggingface.co"

private func directory() throws -> URL {
  let base = try FileManager.default.url(
    for: .applicationSupportDirectory, in: .userDomainMask, appropriateFor: nil, create: true)
  let dir = base.appendingPathComponent("hanzo/models", isDirectory: true)
  try FileManager.default.createDirectory(at: dir, withIntermediateDirectories: true)
  var excluded = URLResourceValues()
  excluded.isExcludedFromBackup = true
  var mutable = dir
  try mutable.setResourceValues(excluded)
  return dir
}

private func path(_ e: Entry) throws -> URL {
  try directory().appendingPathComponent(e.file)
}

private func installed(_ e: Entry) -> Bool {
  guard let p = try? path(e),
    let size = (try? FileManager.default.attributesOfItem(atPath: p.path))?[.size] as? NSNumber
  else { return false }
  return size.int64Value == e.bytes
}

/// Resident memory of this process, in bytes.
private func footprint() -> UInt64 {
  var info = task_vm_info_data_t()
  var count = mach_msg_type_number_t(MemoryLayout<task_vm_info_data_t>.size / MemoryLayout<natural_t>.size)
  let rc = withUnsafeMutablePointer(to: &info) {
    $0.withMemoryRebound(to: integer_t.self, capacity: Int(count)) {
      task_info(mach_task_self_, task_flavor_t(TASK_VM_INFO), $0, &count)
    }
  }
  return rc == KERN_SUCCESS ? UInt64(info.phys_footprint) : 0
}

private final class Tokens {
  let emit: (String) -> Void
  var cancelled = false
  init(_ emit: @escaping (String) -> Void) { self.emit = emit }
}

public class HanzoEngineModule: Module {
  private var cancels: [String: Tokens] = [:]
  private let lock = NSLock()
  private var loaded: String?

  public func definition() -> ModuleDefinition {
    Name("HanzoEngine")
    Events("onProgress", "onToken")

    // Whether the engine is linked into this build.
    Function("available") { () -> Bool in
      #if canImport(HanzoEngineFFI)
      return true
      #else
      return false
      #endif
    }

    Function("memory") { () -> [String: Any] in
      ["total": ProcessInfo.processInfo.physicalMemory, "footprint": footprint()]
    }

    Function("models") { () -> [[String: Any]] in
      let total = ProcessInfo.processInfo.physicalMemory
      return models.map {
        [
          "id": $0.id, "name": $0.name, "bytes": $0.bytes, "memoryGb": $0.memory,
          "installed": installed($0), "fits": total >= UInt64($0.memory) << 30,
        ]
      }
    }

    // Downloads a model into the app's own storage, checked against the host's SHA-256.
    AsyncFunction("download") { (id: String, promise: Promise) in
      guard let e = models.first(where: { $0.id == id }) else {
        promise.reject("E_NO_MODEL", "no such model")
        return
      }
      if installed(e) {
        promise.resolve(nil)
        return
      }
      guard let url = URL(string: "\(host)/\(e.repo)/resolve/main/\(e.file)") else {
        promise.reject("E_URL", "bad model url")
        return
      }
      let session = URLSession(
        configuration: .default, delegate: Progress(id: id, total: e.bytes) { [weak self] done in
          self?.sendEvent("onProgress", ["id": id, "done": done, "total": e.bytes])
        }, delegateQueue: nil)
      session.downloadTask(with: url) { tmp, response, error in
        defer { session.finishTasksAndInvalidate() }
        if let error { return promise.reject("E_DOWNLOAD", error.localizedDescription) }
        guard let tmp, (response as? HTTPURLResponse)?.statusCode == 200 else {
          return promise.reject("E_DOWNLOAD", "the model host refused the download")
        }
        do {
          guard try sha256(tmp) == e.sha256 else {
            try? FileManager.default.removeItem(at: tmp)
            return promise.reject("E_DIGEST", "the download did not match the host's SHA-256")
          }
          let dest = try path(e)
          try? FileManager.default.removeItem(at: dest)
          try FileManager.default.moveItem(at: tmp, to: dest)
          promise.resolve(nil)
        } catch {
          promise.reject("E_DOWNLOAD", error.localizedDescription)
        }
      }.resume()
    }

    AsyncFunction("remove") { (id: String) in
      guard let e = models.first(where: { $0.id == id }), let p = try? path(e) else { return }
      try? FileManager.default.removeItem(at: p)
    }

    // Streams a reply: `onToken` {requestId, text} per piece; resolves with the usage JSON.
    AsyncFunction("chat") { (requestId: String, id: String, messages: String, maxTokens: Int, promise: Promise) in
      #if canImport(HanzoEngineFFI)
      guard let e = models.first(where: { $0.id == id }), installed(e) else {
        return promise.reject("E_NO_MODEL", "that model is not downloaded")
      }
      let tokens = Tokens { [weak self] text in
        self?.sendEvent("onToken", ["requestId": requestId, "text": text])
      }
      self.lock.lock()
      self.cancels[requestId] = tokens
      self.lock.unlock()
      Thread.detachNewThread { [weak self] in
        defer {
          self?.lock.lock()
          self?.cancels[requestId] = nil
          self?.lock.unlock()
        }
        do {
          try self?.open(e)
        } catch {
          return promise.reject("E_LOAD", error.localizedDescription)
        }
        let rc = Self.run(id: e.id, messages: messages, maxTokens: maxTokens, tokens: tokens, promise: promise)
        if rc != 0 { promise.reject("E_CHAT", "the engine failed (\(rc))") }
      }
      #else
      promise.reject("E_UNAVAILABLE", "this build does not include Hanzo Engine")
      #endif
    }

    Function("cancel") { (requestId: String) in
      self.lock.lock()
      self.cancels[requestId]?.cancelled = true
      self.lock.unlock()
    }
  }

  #if canImport(HanzoEngineFFI)
  /// Brings the engine up on `e` (once; the first model is the engine's default).
  private func open(_ e: Entry) throws {
    lock.lock()
    defer { lock.unlock() }
    if loaded == e.id { return }
    let source = try path(e).path
    if loaded == nil {
      setenv("FFI_MODELS", "\(e.id)=gguf:\(source)", 1)
      guard hanzo_ffi_ready() == 1 else { throw NSError(domain: "HanzoEngine", code: 1, userInfo: [NSLocalizedDescriptionKey: "the engine did not start"]) }
    } else {
      let rc = e.id.withCString { n in
        "gguf".withCString { k in
          source.withCString { s in
            hanzo_ffi_load(
              UnsafeRawPointer(n).assumingMemoryBound(to: UInt8.self), strlen(n),
              UnsafeRawPointer(k).assumingMemoryBound(to: UInt8.self), strlen(k),
              UnsafeRawPointer(s).assumingMemoryBound(to: UInt8.self), strlen(s))
          }
        }
      }
      guard rc == 0 else { throw NSError(domain: "HanzoEngine", code: Int(rc), userInfo: [NSLocalizedDescriptionKey: "the model did not load"]) }
    }
    loaded = e.id
  }

  private static func run(id: String, messages: String, maxTokens: Int, tokens: Tokens, promise: Promise) -> Int32 {
    let box = Unmanaged.passRetained(tokens)
    defer { box.release() }
    var usage: UnsafeMutablePointer<UInt8>?
    var usageLen: Int = 0
    let rc = id.utf8CString.withUnsafeBufferPointer { name in
      messages.utf8CString.withUnsafeBufferPointer { body in
        name.baseAddress!.withMemoryRebound(to: UInt8.self, capacity: name.count) { n in
          body.baseAddress!.withMemoryRebound(to: UInt8.self, capacity: body.count) { m in
            hanzo_ffi_chat(
              n, name.count - 1, m, body.count - 1, UInt32(maxTokens),
              { text, len, ctx in
                let t = Unmanaged<Tokens>.fromOpaque(ctx!).takeUnretainedValue()
                if let text { t.emit(String(decoding: UnsafeBufferPointer(start: text, count: len), as: UTF8.self)) }
                return t.cancelled ? 1 : 0
              }, box.toOpaque(), &usage, &usageLen)
          }
        }
      }
    }
    guard rc == 0 else { return rc }
    var report = "{}"
    if let usage {
      report = String(decoding: UnsafeBufferPointer(start: usage, count: usageLen), as: UTF8.self)
      hanzo_ffi_free(usage, usageLen)
    }
    promise.resolve(report)
    return 0
  }
  #endif
}

private final class Progress: NSObject, URLSessionDownloadDelegate {
  let id: String
  let total: Int64
  let report: (Int64) -> Void
  private var last = Date.distantPast
  init(id: String, total: Int64, report: @escaping (Int64) -> Void) {
    self.id = id
    self.total = total
    self.report = report
  }
  func urlSession(_ s: URLSession, downloadTask: URLSessionDownloadTask, didWriteData _: Int64, totalBytesWritten done: Int64, totalBytesExpectedToWrite _: Int64) {
    if Date().timeIntervalSince(last) > 0.25 {
      last = Date()
      report(done)
    }
  }
  func urlSession(_ s: URLSession, downloadTask: URLSessionDownloadTask, didFinishDownloadingTo _: URL) {}
}

private func sha256(_ url: URL) throws -> String {
  let handle = try FileHandle(forReadingFrom: url)
  defer { try? handle.close() }
  var hasher = SHA256()
  while let chunk = try handle.read(upToCount: 1 << 20), !chunk.isEmpty { hasher.update(data: chunk) }
  return hasher.finalize().map { String(format: "%02x", $0) }.joined()
}
