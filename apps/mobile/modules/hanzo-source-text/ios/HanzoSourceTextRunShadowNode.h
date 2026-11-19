#pragma once

#include <react/renderer/components/HanzoSourceTextSpec/EventEmitters.h>
#include <react/renderer/components/HanzoSourceTextSpec/Props.h>
#include <react/renderer/components/HanzoSourceTextSpec/States.h>
#include <react/renderer/components/view/ConcreteViewShadowNode.h>

namespace facebook::react {
extern const char HanzoSourceTextRunComponentName[];

using HanzoSourceTextRunShadowNode =
    ConcreteViewShadowNode<HanzoSourceTextRunComponentName, HanzoSourceTextRunProps,
                           HanzoSourceTextRunEventEmitter, HanzoSourceTextRunState>;
} // namespace facebook::react
