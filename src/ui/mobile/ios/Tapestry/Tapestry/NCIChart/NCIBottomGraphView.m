//
//  NCIBottomGraphView.m
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIBottomGraphView.h"
#import "NCIHandspikeView.h"

@interface NCIBottomGraphView(){
    NCIHandspikeView *handspikeLeft;
    NCIHandspikeView *handspikeRight;
    UIView *rightAmputation;
    UIView *leftAmputation;
    float xHandspikeLeft;
    float xHandspikeRight;
}

@end

@implementation NCIBottomGraphView

- (id)initWithChart: (NCIChartView *)chartHolder{
    self = [super initWithChart:chartHolder];
    if (self){
        self.chart = chartHolder;
        self.hasGrid = NO;
        self.hasYLabels = NO;
        
        xHandspikeLeft = -1;
        xHandspikeRight = -1;
        
        leftAmputation = [[UIView alloc] initWithFrame:CGRectZero];
        leftAmputation.backgroundColor = [[UIColor blueColor] colorWithAlphaComponent:0.2];
        [self addSubview:leftAmputation];
        
        handspikeLeft = [[NCIHandspikeView alloc] initWithFrame:CGRectZero];
        [self addSubview:handspikeLeft];
        
        rightAmputation = [[UIView alloc] initWithFrame:CGRectZero];
        rightAmputation.backgroundColor = [[UIColor blueColor] colorWithAlphaComponent:0.2];
        [self addSubview:rightAmputation];
        
        handspikeRight = [[NCIHandspikeView alloc] initWithFrame:CGRectZero];
        [self addSubview:handspikeRight];
    }
    return self;
}

- (void)layoutSubviews{
    [super layoutSubviews];
    int handspikeWidth = 32;
    
    if (xHandspikeLeft < 0){
        if (self.chart.minRangeDate){
            xHandspikeLeft = self.leftRightIndent - handspikeWidth/2;;
        } else {
            xHandspikeLeft = self.leftRightIndent - handspikeWidth/2;
        }
    };
    
    if (xHandspikeRight < 0){
        if (self.chart.maxRangeDate){
            xHandspikeRight = self.frame.size.width - self.leftRightIndent - handspikeWidth/2;
        } else {
            xHandspikeRight = self.frame.size.width - self.leftRightIndent - handspikeWidth/2;
        }
    };
    
    if (xHandspikeLeft < self.leftRightIndent - handspikeWidth/2){
        xHandspikeLeft = self.leftRightIndent - handspikeWidth/2;
    }
    
    if (xHandspikeRight > self.frame.size.width - self.leftRightIndent - handspikeWidth/2){
        xHandspikeRight = self.frame.size.width - self.leftRightIndent - handspikeWidth/2;
    }
    
    if (xHandspikeRight - xHandspikeLeft < 50)
        return;
    
    
    handspikeLeft.frame = CGRectMake(xHandspikeLeft, 0, handspikeWidth, self.frame.size.height - self.bottomChartIndent);
    leftAmputation.frame = CGRectMake(self.leftRightIndent, 0, xHandspikeLeft - self.leftRightIndent + handspikeWidth/2, self.frame.size.height - self.bottomChartIndent);
    
    handspikeRight.frame = CGRectMake(xHandspikeRight, 0, handspikeWidth, self.frame.size.height - self.bottomChartIndent);
    rightAmputation.frame = CGRectMake(xHandspikeRight + handspikeWidth/2, 0,
                                       self.frame.size.width - xHandspikeRight - self.leftRightIndent - handspikeWidth/2,
                                       self.frame.size.height - self.bottomChartIndent);
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
