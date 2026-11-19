#import <React/RCTUIManager.h>
#import <React/RCTViewManager.h>

@interface HanzoSourceTextManager : RCTViewManager
@end

@implementation HanzoSourceTextManager

RCT_EXPORT_MODULE(HanzoSourceText)

- (UIView *)view {
  return [[UIView alloc] init];
}

@end

@interface HanzoSourceTextRunManager : RCTViewManager
@end

@implementation HanzoSourceTextRunManager

RCT_EXPORT_MODULE(HanzoSourceTextRun)

- (UIView *)view {
  return nil;
}

@end
