//
//  NCIGraphView.m
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGraphView.h"
#import "NCIGridAreaView.h"
#import "NCIBottomGraphView.h"

@interface NCIGraphView()<UIScrollViewDelegate>{
    NSMutableArray *yAxisLabels;
    int minXVal;
    int maxXVal;
}

@end

@implementation NCIGraphView

- (id)initWithChart: (NCIChartView *)chartHolder{
    self = [self initWithFrame:CGRectZero];
    if (self){
        self.scaleIndex = 1;
        self.chart = chartHolder;
        self.hasGrid = YES;
        self.hasYLabels = YES;
        self.topChartIndent =  15;
        _gridScroll = [[UIScrollView alloc] init];
        _gridScroll.delegate = self;
        [self addSubview:_gridScroll];
        _gridScroll.showsHorizontalScrollIndicator = NO;
        _gridArea = [[NCIGridAreaView alloc] initWithChart:self.chart];
        [_gridScroll addSubview:_gridArea];
    }
    return self;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        
        
    }
    return self;
}

- (void)layoutSubviews{
    
    if (!yAxisLabels){
        _bottomChartIndent = 60;
        _leftRightIndent = 60;
        
        int yLabelsCount  = (self.bounds.size.height - _bottomChartIndent - _topChartIndent)/50;
        if (yLabelsCount < 2)
            yLabelsCount = 2;
        yAxisLabels = [[NSMutableArray alloc] initWithCapacity:yLabelsCount];
        
        int ind = 0;
        for (ind = 0; ind< yLabelsCount; ind++){
            UILabel *yLabel = [[UILabel alloc] initWithFrame:CGRectZero];
            yLabel.backgroundColor = [UIColor clearColor];
            [yAxisLabels addObject:yLabel];
            [self addSubview:yLabel];
        };

    };
    
    int ind;
    for (ind = 0; ind< yAxisLabels.count; ind++){
        UILabel *yLabel = yAxisLabels[ind];
        yLabel.frame = CGRectMake(10, _topChartIndent + ind*(self.bounds.size.height - _bottomChartIndent - _topChartIndent)/(yAxisLabels.count - 1),50, 20);
    }
    
}

- (void)detectXRange {
    minXVal = self.chart.minXVal;
    maxXVal = self.chart.maxXVal;
}

- (void)drawRect:(CGRect)rect {
    [self detectXRange];
    
    float yFork = self.chart.maxYVal - self.chart.minYVal;

    UIBezierPath *path = [UIBezierPath bezierPath];
    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    CGContextSetLineCap(currentContext, kCGLineCapRound);
    CGContextSetLineWidth(currentContext, 0.5);
    CGContextSetLineJoin(currentContext, kCGLineJoinRound);
    CGContextBeginPath(currentContext);
    [[UIColor blueColor] setStroke];
    CGContextAddPath(currentContext, path.CGPath);
    CGContextDrawPath(currentContext, kCGPathStroke);
    [[UIColor blackColor] setStroke];
    
    int ind = 0;
    
    CGFloat dashes[] = { 1, 1 };
    CGContextSetLineDash(currentContext, 0.0,  dashes , 2 );
    if (self.chart.maxYVal && self.chart.minYVal){
        for (ind = 0; ind < yAxisLabels.count; ind++){
            UILabel *yLabel = yAxisLabels[ind];
            if (self.hasYLabels){
                yLabel.text = [NSString stringWithFormat:@"%.1f", self.chart.maxYVal - ind * yFork/(yAxisLabels.count - 1)];
            }
            if (self.hasGrid || ind == yAxisLabels.count -1){
                CGContextMoveToPoint(currentContext, yLabel.frame.origin.x + _leftRightIndent/2, yLabel.frame.origin.y);
                CGContextAddLineToPoint(currentContext, self.frame.size.width - _leftRightIndent/2, yLabel.frame.origin.y);
                CGContextStrokePath(currentContext);
            }
        };
    };

    
    [_gridArea setNeedsDisplay];
    
}

@end
