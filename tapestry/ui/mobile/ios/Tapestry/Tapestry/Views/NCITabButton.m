//
//  NCITabButton.m
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCITabButton.h"

@interface NCITabButton(){
    UIColor *btnColor;
    UIColor *btnSelectedColor;
}

@end

@implementation NCITabButton

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        btnColor = [UIColor colorWithRed:95/255.0 green:116/255.0 blue:126/255.0 alpha:1];
        btnSelectedColor = [UIColor colorWithRed:95/255.0 green:116/255.0 blue:126/255.0 alpha:0.7];
        self.backgroundColor = btnColor;
        [self addTarget:self action:@selector(tapSelf) forControlEvents:UIControlEventTouchUpInside];
    }
    return self;
}

- (void)tapSelf{
    self.selectAction();
}
//
- (void)setSelected:(BOOL)selected{
    [super setSelected:selected];
    self.backgroundColor = selected ? btnSelectedColor : btnColor;
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
}
*/

@end
