//
//  NCIPeriodSwitcher.h
//  Tapestry
//
//  Created by Infoblox Inc on 11/21/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import <UIKit/UIKit.h>

@interface NCIPeriodSwitcher : UIButton

- (id)initWithFrame:(CGRect)frame label:(NSString *)label gap:(int) periodGap andAction:(void (^)(void))action;
- (void)deselect;

@property(nonatomic)int period;


@end
