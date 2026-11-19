Pod::Spec.new do |s|
  s.name = 'HanzoVoiceTranscription'
  s.version = '1.0.0'
  s.summary = 'Local recording conversion for Hanzo Dev dictation.'
  s.author = 'Hanzo AI'
  s.homepage = 'https://hanzodev.com'
  s.platform = :ios, '18.0'
  s.source = { :path => '.' }
  s.static_framework = true
  s.dependency 'ExpoModulesCore'
  s.source_files = '*.swift'
  s.frameworks = 'AudioToolbox'
end
