//
//  NCIBottomGraphView.m
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIBottomGraphView.h"
#import "NCIHandspikeView.h"
#import "NCIMianGraphView.h"

@interface NCIBottomGraphView(){
    NCIHandspikeView *handspikeLeft;
    NCIHandspikeView *handspikeRight;
    UIView *rightAmputation;
    UIView *leftAmputation;
    
    int handspikeWidth;
    float gridWidth;
    float gridStep; //number of seconds for pixel
    
    //values on start dragging ranges
    float fixedLeftVal;
    float fixedRightVal;
}

@property(nonatomic)float xHandspikeLeft;
@property(nonatomic)float xHandspikeRight;

@end

@implementation NCIBottomGraphView

- (id)initWithChart: (NCIChartView *)chartHolder{
    self = [super initWithChart:chartHolder];
    if (self){
        self.chart = chartHolder;
        self.hasGrid = NO;
        self.hasYLabels = NO;
        self.topChartIndent = 15;
        
        leftAmputation = [[UIView alloc] initWithFrame:CGRectZero];
        leftAmputation.backgroundColor = [UIColor colorWithWhite:0.3 alpha:0.1];
        [self addSubview:leftAmputation];
        
        handspikeLeft = [[NCIHandspikeView alloc] initWithFrame:CGRectZero];
        [self addSubview:handspikeLeft];
        
        rightAmputation = [[UIView alloc] initWithFrame:CGRectZero];
        rightAmputation.backgroundColor = [UIColor colorWithWhite:0.3 alpha:0.1];
        [self addSubview:rightAmputation];
        
        handspikeRight = [[NCIHandspikeView alloc] initWithFrame:CGRectZero];
        [self addSubview:handspikeRight];
        
        handspikeWidth = 32;
    }
    return self;
}

- (void)layoutSubviews{
    [super layoutSubviews];
    
    self.scaleIndex = 1;
    self.gridScroll.frame = CGRectMake(self.leftRightIndent,self.topChartIndent, self.frame.size.width - 2*self.leftRightIndent,
                                       self.frame.size.height - self.topChartIndent);
    self.gridScroll.contentSize =
    CGSizeMake((self.frame.size.width - 2*self.leftRightIndent)*self.scaleIndex, self.frame.size.height - self.topChartIndent - self.bottomChartIndent);
    self.gridArea.frame = CGRectMake(0, 0, (self.frame.size.width - 2*self.leftRightIndent)*self.scaleIndex, self.frame.size.height - self.topChartIndent - self.bottomChartIndent);
    [self.gridScroll setContentOffset:CGPointMake((self.scaleIndex -1)*(self.frame.size.width - 2*self.leftRightIndent - self.leftShift*self.scaleIndex), 0)];
    
    [self redrawRanges];

}

- (void)redrawRanges{
    gridWidth = self.frame.size.width - 2*self.leftRightIndent;
    gridStep = gridWidth/(self.chart.maxXVal - self.chart.minXVal);
    float handspikeIndent = self.leftRightIndent - handspikeWidth/2;
    float gridHeigth = self.frame.size.height - self.bottomChartIndent - self.topChartIndent;
    
    if (self.chart.chartData.count < 1)
        return;
    
    if (!_xHandspikeLeft){
        _xHandspikeLeft = handspikeIndent;
        fixedLeftVal = _xHandspikeLeft;
    };
    
    if (!_xHandspikeRight){
        _xHandspikeRight = handspikeIndent + gridWidth ;
        fixedRightVal = _xHandspikeRight;
    };
    
    if (self.chart.minRangeDate ){
        _xHandspikeLeft = handspikeIndent + ([self.chart.minRangeDate timeIntervalSince1970] - self.chart.minXVal)*gridStep;
        _xHandspikeRight = handspikeIndent + ([self.chart.maxRangeDate timeIntervalSince1970] - self.chart.minXVal)*gridStep;
    }
    
    if (_xHandspikeLeft < handspikeIndent){
        _xHandspikeLeft = handspikeIndent;
    }
    
    if (_xHandspikeRight > gridWidth + handspikeIndent){
        _xHandspikeRight = gridWidth + handspikeIndent;
    }
    
    if (_xHandspikeRight - _xHandspikeLeft < 0)
        return;
    
    
    handspikeLeft.frame = CGRectMake(_xHandspikeLeft, self.topChartIndent, handspikeWidth, gridHeigth);
    leftAmputation.frame = CGRectMake(self.leftRightIndent, self.topChartIndent, _xHandspikeLeft - handspikeIndent, gridHeigth);
    
    handspikeRight.frame = CGRectMake(_xHandspikeRight, self.topChartIndent, handspikeWidth, gridHeigth);
    rightAmputation.frame = CGRectMake(_xHandspikeRight + handspikeWidth/2, self.topChartIndent,
                                       gridWidth + handspikeIndent - _xHandspikeRight, gridHeigth);
    
}

float startX = 0;

-(void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    UITouch *touch = [[event allTouches] anyObject];
    
    if( [touch view] == handspikeLeft)
    {
        CGPoint location = [touch locationInView:self];
        startX = location.x - handspikeLeft.center.x;
    } else if ([touch view] == handspikeRight){
        CGPoint location = [touch locationInView:self];
        startX = location.x - handspikeRight.center.x;
    }
}
- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
    //here we set up new min and max ranges values for chart
    UITouch *touch = [[event allTouches] anyObject];
    if( [touch view] == handspikeLeft)
    {
        CGPoint location = [touch locationInView:self];
        if (_xHandspikeRight - (location.x - startX - self.leftRightIndent) < 35)
            return;
        
        self.chart.minRangeDate = [NSDate dateWithTimeIntervalSince1970:
                                   self.chart.minXVal + (location.x - startX - self.leftRightIndent)/gridStep];
        [self.chart.mainGraph setNeedsLayout];
        [self.chart.mainGraph setNeedsDisplay];
     //   [self redrawRanges];
    } else if ([touch view] == handspikeRight){
        
        CGPoint location = [touch locationInView:self];
        [self.chart.mainGraph setNeedsLayout];
        [self.chart.mainGraph setNeedsDisplay];
        self.chart.maxRangeDate = [NSDate dateWithTimeIntervalSince1970:self.chart.minXVal + (location.x - startX - self.leftRightIndent)/gridStep];
        //[self redrawRanges];
    }
}

- (void)touchesEnded:(NSSet *)touches withEvent:(UIEvent *)event{
    
    if (fixedLeftVal != _xHandspikeLeft || fixedRightVal != _xHandspikeRight){
        [self.chart.mainGraph setNeedsLayout];
        [self.chart.mainGraph setNeedsDisplay];
        
        fixedLeftVal = _xHandspikeLeft ;
        fixedRightVal = _xHandspikeRight;
    }
    
}


@end
