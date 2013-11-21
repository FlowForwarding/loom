//
//  NCIHandspikeView.m
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIHandspikeView.h"

@interface NCIHandspikeView(){
    UIView *border;
}
@end

@implementation NCIHandspikeView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        border = [[UIView alloc] initWithFrame:CGRectZero];
        border.backgroundColor = [UIColor blackColor];
        [self addSubview:border];
        self.backgroundColor = [UIColor colorWithWhite:0.5 alpha:0.5];
    }
    return self;
}

-(void)layoutSubviews{
        border.frame = CGRectMake(self.frame.size.width/2 - 1, 0, 2, self.frame.size.height);
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
