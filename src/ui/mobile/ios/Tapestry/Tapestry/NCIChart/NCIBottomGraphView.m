//
//  NCIBottomGraphView.m
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIBottomGraphView.h"

@interface NCIBottomGraphView(){
    UIView *handspikeLeft;
    UIView *handspikeRight;
    UIView *rightAmputation;
    UIView *leftAmputation;
}

@end

@implementation NCIBottomGraphView

- (id)initWithChart: (NCIChartView *)chartHolder{
    self = [super initWithChart:chartHolder];
    if (self){
        self.chart = chartHolder;
        self.hasGrid = NO;
        self.hasYLabels = NO;
        
        leftAmputation = [[UIView alloc] initWithFrame:CGRectZero];
        leftAmputation.backgroundColor = [[UIColor blueColor] colorWithAlphaComponent:0.3];
        [self addSubview:leftAmputation];
        
        handspikeLeft = [[UIView alloc] initWithFrame:CGRectZero];
        handspikeLeft.backgroundColor = [UIColor blackColor];
        [self addSubview:handspikeLeft];
        
        rightAmputation = [[UIView alloc] initWithFrame:CGRectZero];
        rightAmputation.backgroundColor = [[UIColor blueColor] colorWithAlphaComponent:0.3];
        [self addSubview:rightAmputation];
        
        handspikeRight = [[UIView alloc] initWithFrame:CGRectZero];
        handspikeRight.backgroundColor = [UIColor blackColor];
        [self addSubview:handspikeRight];
    }
    return self;
}

float xHandspikeLeft = 100;
float xHandspikeRight = 400;

- (void)layoutSubviews{
    [super layoutSubviews];
    if (xHandspikeLeft < self.leftRightIndent){
        xHandspikeLeft = self.leftRightIndent;
    }
    
    if (xHandspikeRight > self.frame.size.width - self.leftRightIndent){
        xHandspikeRight = self.frame.size.width - self.leftRightIndent;
    }
    
    if (xHandspikeRight - xHandspikeLeft < 50)
        return;
    
    handspikeLeft.frame = CGRectMake(xHandspikeLeft, 0, 15, self.frame.size.height - self.bottomChartIndent);
    leftAmputation.frame = CGRectMake(self.leftRightIndent, 0, xHandspikeLeft - self.leftRightIndent, self.frame.size.height - self.bottomChartIndent);
    
    handspikeRight.frame = CGRectMake(xHandspikeRight, 0, 15, self.frame.size.height - self.bottomChartIndent);
    rightAmputation.frame = CGRectMake(xHandspikeRight, 0, self.frame.size.width - xHandspikeRight - self.leftRightIndent, self.frame.size.height - self.bottomChartIndent);
}

float startX = 0;
float startY = 0;

-(void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    UITouch *touch = [[event allTouches] anyObject];
    
    if( [touch view] == handspikeLeft)
    {
        CGPoint location = [touch locationInView:self];
        startX = location.x - handspikeLeft.center.x;
        startY = handspikeLeft.center.y;
    } else if ([touch view] == handspikeRight){
        CGPoint location = [touch locationInView:self];
        startX = location.x - handspikeRight.center.x;
        startY = handspikeRight.center.y;
    }
}
- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
    UITouch *touch = [[event allTouches] anyObject];
    if( [touch view] == handspikeLeft)
    {
        CGPoint location = [touch locationInView:self];
        xHandspikeLeft = location.x - startX;
        [self setNeedsLayout];
    } else if ([touch view] == handspikeRight){
        CGPoint location = [touch locationInView:self];
        xHandspikeRight = location.x - startX;
        [self setNeedsLayout];
    }
}


@end
