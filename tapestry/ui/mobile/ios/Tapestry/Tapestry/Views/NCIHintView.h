//
//  NCIHintView.h
//  Tapestry
//
//  Created by Infoblox Inc on 11/25/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import <UIKit/UIKit.h>

@interface NCIHintView : UIView

+ (id)globaHint;
- (void)showHintWithText:(NSString *)text andPoint:(CGPoint)point;

@end
