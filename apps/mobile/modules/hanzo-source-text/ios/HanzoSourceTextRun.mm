#import "HanzoSourceTextRun.h"
#import <React/RCTFabricComponentsPlugins.h>
#import "HanzoSourceTextRunComponentDescriptor.h"
#import <react/renderer/components/HanzoSourceTextSpec/Props.h>
#import <react/renderer/components/HanzoSourceTextSpec/RCTComponentViewHelpers.h>

using namespace facebook::react;

@interface HanzoSourceTextRun () <RCTT3SourceTextRunViewProtocol>

@end

@implementation HanzoSourceTextRun {
  NSString *_text;
}

+ (ComponentDescriptorProvider)componentDescriptorProvider {
  return concreteComponentDescriptorProvider<
      HanzoSourceTextRunComponentDescriptor>();
}

- (instancetype)initWithFrame:(CGRect)frame {
  if (self = [super initWithFrame:frame]) {
    static const auto defaultProps =
        std::make_shared<const HanzoSourceTextRunProps>();
    _props = defaultProps;
  }
  return self;
}

- (void)updateProps:(Props::Shared const &)props
           oldProps:(Props::Shared const &)oldProps {
  const auto &oldViewProps =
      *std::static_pointer_cast<HanzoSourceTextRunProps const>(_props);
  const auto &newViewProps =
      *std::static_pointer_cast<HanzoSourceTextRunProps const>(props);

  if (newViewProps.text != oldViewProps.text) {
    NSString *text = [NSString stringWithUTF8String:newViewProps.text.c_str()];
    _text = text;
  }

  [super updateProps:props oldProps:oldProps];
}

+ (BOOL)shouldBeRecycled {
  return NO;
}

Class<RCTComponentViewProtocol> HanzoSourceTextRunCls(void) {
  return HanzoSourceTextRun.class;
}

@end
