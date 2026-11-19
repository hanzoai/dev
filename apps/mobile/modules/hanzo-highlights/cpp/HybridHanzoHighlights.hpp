#pragma once

#include "HybridHanzoHighlightsSpec.hpp"

namespace margelo::nitro::hanzohighlights {

class HybridHanzoHighlights : public HybridHanzoHighlightsSpec {
public:
  HybridHanzoHighlights() : HybridObject(TAG) {}

  std::optional<std::shared_ptr<ArrayBuffer>> tokenize(SourceText source,
                                                       double language) override;
};

} // namespace margelo::nitro::hanzohighlights
