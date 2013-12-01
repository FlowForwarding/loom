//
//  NCIPeriodSwitcher.h
//  Tapestry
//
//  Created by Ira on 11/21/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface NCIPeriodSwitcher : UIButton

- (id)initWithFrame:(CGRect)frame label:(NSString *)label gap:(int) periodGap andAction:(void (^)(void))action;
- (void)deselect;

@property(nonatomic)int period;


@end
