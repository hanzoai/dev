#pragma once

#include "HanzoSourceTextRunShadowNode.h"

#include <react/renderer/componentregistry/ComponentDescriptorProviderRegistry.h>
#include <react/renderer/core/ConcreteComponentDescriptor.h>

namespace facebook::react {
using HanzoSourceTextRunComponentDescriptor =
    ConcreteComponentDescriptor<HanzoSourceTextRunShadowNode>;

void HanzoSourceTextRunSpec_registerComponentDescriptorsFromCodegen(
    std::shared_ptr<const ComponentDescriptorProviderRegistry> registry);
} // namespace facebook::react
