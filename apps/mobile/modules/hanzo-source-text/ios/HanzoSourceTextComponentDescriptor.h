#pragma once

#include "HanzoSourceTextShadowNode.h"

#include <react/renderer/componentregistry/ComponentDescriptorProviderRegistry.h>
#include <react/renderer/core/ConcreteComponentDescriptor.h>

namespace facebook::react {
using HanzoSourceTextComponentDescriptor =
    ConcreteComponentDescriptor<HanzoSourceTextShadowNode>;

void HanzoSourceTextSpec_registerComponentDescriptorsFromCodegen(
    std::shared_ptr<const ComponentDescriptorProviderRegistry> registry);
} // namespace facebook::react
