# Hanzo Engine, in the app: `build.sh` makes HanzoEngineFFI.xcframework (aarch64-apple-ios and
# aarch64-apple-ios-sim staticlibs of hanzo-engine-ffi, Metal). Without it the module builds and reports
# `available() == false`, so the app still ships; with it, the model runs on this phone.
Pod::Spec.new do |s|
  s.name = 'HanzoEngine'
  s.version = '1.0.0'
  s.summary = 'Hanzo Engine on the device: private local chat with a Zen model.'
  s.author = 'Hanzo AI'
  s.homepage = 'https://hanzo.ai'
  s.platform = :ios, '18.0'
  s.source = { :path => '.' }
  s.static_framework = true
  s.dependency 'ExpoModulesCore'
  s.source_files = '*.swift'
  s.frameworks = 'Metal', 'MetalKit', 'Accelerate', 'Security', 'SystemConfiguration', 'CoreFoundation'
  s.libraries = 'c++'
  xcframework = File.join(__dir__, 'HanzoEngineFFI.xcframework')
  s.vendored_frameworks = 'HanzoEngineFFI.xcframework' if File.directory?(xcframework)
end
