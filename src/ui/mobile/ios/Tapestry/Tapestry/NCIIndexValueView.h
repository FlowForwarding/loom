//
//  NCIIndexValueView.h
//  Tapestry
//
//  Created by Ira on 11/12/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface NCIIndexValueView : UIView


- (id)initWithFrame:(CGRect)frame indName:(NSString *)indName indSize:(float)size;
- (void)setIndValue:(NSString *)value withDate:(NSString *)date;
- (void)setTooltipText:(NSString *)text;
- (void)resetData;

@end
