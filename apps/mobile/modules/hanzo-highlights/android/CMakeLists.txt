cmake_minimum_required(VERSION 3.22.1)

project(hanzohighlights LANGUAGES C CXX)

set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(HANZO_HIGHLIGHTS_ROOT "${CMAKE_SOURCE_DIR}/..")

add_library(
  hanzohighlights SHARED
  src/main/cpp/cpp-adapter.cpp
  "${HANZO_HIGHLIGHTS_ROOT}/cpp/HybridHanzoHighlights.cpp"
  "${HANZO_HIGHLIGHTS_ROOT}/cpp/Utf16ToUtf8.cpp"
  "${HANZO_HIGHLIGHTS_ROOT}/cpp/hanzo_highlights.c"
  "${HANZO_HIGHLIGHTS_ROOT}/Vendor/highlights/highlights.c"
  "${HANZO_HIGHLIGHTS_ROOT}/Vendor/wasm2c/wasm-rt-impl.c"
  "${HANZO_HIGHLIGHTS_ROOT}/Vendor/wasm2c/wasm-rt-mem-impl.c"
)

include(${CMAKE_SOURCE_DIR}/../nitrogen/generated/android/hanzohighlights+autolinking.cmake)

target_include_directories(
  hanzohighlights
  PRIVATE "${HANZO_HIGHLIGHTS_ROOT}/cpp" "${HANZO_HIGHLIGHTS_ROOT}/Vendor"
          "${HANZO_HIGHLIGHTS_ROOT}/Vendor/highlights" "${HANZO_HIGHLIGHTS_ROOT}/Vendor/wasm2c"
)

# Plain malloc memory with explicit bounds checks: no guard-page signal
# handlers competing with ART's.
target_compile_definitions(
  hanzohighlights PRIVATE WASM_RT_USE_MMAP=0 WASM_RT_MEMCHECK_BOUNDS_CHECK=1
                       WASM_RT_SKIP_SIGNAL_RECOVERY=1
)

# The lexers are generated code; keep them fast in debug builds too.
target_compile_options(hanzohighlights PRIVATE -O2)

target_link_options(
  hanzohighlights PRIVATE "-Wl,-z,common-page-size=16384" "-Wl,-z,max-page-size=16384"
)
