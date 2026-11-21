// swift-tools-version: 6.0
import PackageDescription

let package = Package(
  name: "HanzoMenubar",
  platforms: [.macOS(.v14)],
  targets: [
    .executableTarget(name: "HanzoMenubar", path: "Sources/HanzoMenubar")
  ]
)
