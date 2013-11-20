//
//  NCIChartView.m
//  Tapestry
//
//  Created by Ira on 11/13/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIChartView.h"
#import "NCIGraphView.h"
#import "NCIBottomGraphView.h"

@interface NCIChartView(){
    
    bool hasSlider;
}

@end

@implementation NCIChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        hasSlider = YES;
        self.chartData = [[NSMutableArray alloc] init];
        _mainGraph = [[NCIGraphView alloc] initWithChart:self];
        _mainGraph.backgroundColor = [UIColor whiteColor];
        [self addSubview:_mainGraph];
        
        _bottomGraph = [[NCIBottomGraphView alloc] initWithChart:self];
        _bottomGraph.backgroundColor = [UIColor whiteColor];
        [self addSubview:_bottomGraph];

    }
    return self;
}

- (void)resetChart{
    [self.chartData removeAllObjects];
}

- (void)layoutSubviews{
    float bottomGraphHeight = 130;
    if (hasSlider){
        _mainGraph.frame = CGRectMake(0, 0, self.bounds.size.width, self.bounds.size.height - bottomGraphHeight);
        _bottomGraph.frame = CGRectMake(0, self.bounds.size.height - bottomGraphHeight, self.bounds.size.width, bottomGraphHeight);
    } else {
        _mainGraph.frame = self.bounds;
    }
    [_mainGraph setNeedsDisplay];
    [_bottomGraph setNeedsDisplay];
}

- (void)setMinX:(NSDate *)date{
    _minXVal  = [date timeIntervalSince1970];
}

- (void)setMaxX:(NSDate *)date{
    _maxXVal = [date timeIntervalSince1970];
}

- (void)setRanges:(NSDate *)min max:(NSDate *)max{
    float xFork = self.maxXVal - self.minXVal;
    float xStep = (self.bounds.size.width - _bottomGraph.leftRightIndent*2)/xFork;
    _bottomGraph.xHandspikeLeft = self.frame.size.width -  _bottomGraph.leftRightIndent - (self.maxXVal - [min timeIntervalSince1970])*xStep;
    _bottomGraph.xHandspikeRight = self.frame.size.width -  _bottomGraph.leftRightIndent - (self.maxXVal - [max timeIntervalSince1970])*xStep;
    _mainGraph.scaleIndex = xFork/([max timeIntervalSince1970] - [min timeIntervalSince1970]);
    NSLog(@"%f", _mainGraph.scaleIndex);
}

- (void)addPoint:(NSDate *)date val:(NSString *)value{

    int dateSeconds = [date timeIntervalSince1970];
    if (!_minXVal || _minXVal > dateSeconds){
        _minXVal = dateSeconds;
    };
    if (!_maxXVal || _maxXVal < dateSeconds){
        _maxXVal = dateSeconds;
    };
    
    int indexValue = [value integerValue];
    if (!_minYVal || _minYVal > indexValue){
        _minYVal = indexValue;
    };
    if (!_maxYVal || _maxYVal < indexValue){
        _maxYVal = indexValue;
    };
    
    [self.chartData addObject:@[date, value]];
}

- (void)drawChart{
    [_mainGraph setNeedsDisplay];
    [_mainGraph setNeedsLayout];
    [_bottomGraph setNeedsDisplay];
    [_bottomGraph setNeedsLayout];
}

@end
