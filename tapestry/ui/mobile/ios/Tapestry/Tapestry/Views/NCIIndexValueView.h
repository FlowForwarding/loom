//
//  NCIIndexValueView.h
//  Tapestry
//
//  Created by Infoblox Inc on 11/12/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import <UIKit/UIKit.h>

@interface NCIIndexValueView : UIView


- (id)initWithFrame:(CGRect)frame indName:(NSString *)indName indSize:(float)size;
- (void)setIndValue:(NSString *)value withDate:(NSString *)date;
- (void)setTooltipText:(NSString *)text;
- (void)resetData;

@property(nonatomic, strong)NSString *dateServerString;

@end
