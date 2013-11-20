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
    
    //todo not to redraw on initial ranges setup
    float fixedLeftVal;
    float fixedRightVal;
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
    
    if (!_xHandspikeLeft){
        //todo simplify all of these calculations!! on th bottom to
        _xHandspikeLeft = self.leftRightIndent - handspikeWidth/2;
        fixedLeftVal = _xHandspikeLeft;
    };
    
    if (!_xHandspikeRight){
        _xHandspikeRight = self.frame.size.width - self.leftRightIndent - handspikeWidth/2;
        fixedRightVal = _xHandspikeRight;
    };
    
    if (_xHandspikeLeft < self.leftRightIndent - handspikeWidth/2){
        _xHandspikeLeft = self.leftRightIndent - handspikeWidth/2;
    }
    
    if (_xHandspikeRight > self.frame.size.width - self.leftRightIndent - handspikeWidth/2){
        _xHandspikeRight = self.frame.size.width - self.leftRightIndent - handspikeWidth/2;
    }
    
    if (_xHandspikeRight - _xHandspikeLeft < 30)
        return;
    
    
    handspikeLeft.frame = CGRectMake(_xHandspikeLeft, 0, handspikeWidth, self.frame.size.height - self.bottomChartIndent);
    leftAmputation.frame = CGRectMake(self.leftRightIndent, 0, _xHandspikeLeft - self.leftRightIndent + handspikeWidth/2, self.frame.size.height - self.bottomChartIndent);
    
    handspikeRight.frame = CGRectMake(_xHandspikeRight, 0, handspikeWidth, self.frame.size.height - self.bottomChartIndent);
    rightAmputation.frame = CGRectMake(_xHandspikeRight + handspikeWidth/2, 0,
                                       self.frame.size.width - _xHandspikeRight - self.leftRightIndent - handspikeWidth/2,
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
        _xHandspikeLeft = location.x - startX;
        [self setNeedsLayout];
    } else if ([touch view] == handspikeRight){
        CGPoint location = [touch locationInView:self];
        _xHandspikeRight = location.x - startX;
        [self setNeedsLayout];
    }
}



- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event{
    
//    _xHandspikeLeft = self.leftRightIndent - handspikeWidth/2;
//    _xHandspikeRight = self.frame.size.width - self.leftRightIndent - handspikeWidth/2;
    
    if (fixedLeftVal != _xHandspikeLeft || fixedRightVal != _xHandspikeRight){
        float xlength = self.frame.size.width - 2 *self.leftRightIndent;
        float leftFloat = _xHandspikeLeft - self.leftRightIndent;
        float rightFloat = _xHandspikeRight - self.leftRightIndent;
        
        self.chart.mainGraph.leftShift = leftFloat;
        
        self.chart.mainGraph.scaleIndex = xlength/(rightFloat - leftFloat);
       
        
        [self.chart.mainGraph setNeedsLayout];
        [self.chart.mainGraph setNeedsDisplay];
        
        fixedLeftVal = _xHandspikeLeft ;
        fixedRightVal = _xHandspikeRight;
    }
    
}


@end
