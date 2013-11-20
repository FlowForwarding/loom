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
    float topChartIndent;
    int minXVal;
    int maxXVal;
    NCIGridAreaView *gridArea;
    UIScrollView *gridScroll;
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
        gridScroll = [[UIScrollView alloc] init];
        gridScroll.delegate = self;
        [self addSubview:gridScroll];
        gridScroll.showsHorizontalScrollIndicator = NO;
        gridArea = [[NCIGridAreaView alloc] initWithChart:self.chart];
        [gridScroll addSubview:gridArea];
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

-(void)scrollViewDidScroll:(UIScrollView *)scrollView{
    //TODO clean up this!!! not in general logic
    if (self.scaleIndex != 1){
        float handspikeDiff = self.chart.bottomGraph.xHandspikeRight  - self.chart.bottomGraph.xHandspikeLeft;
        float newLeftHandsptipeX = (scrollView.contentOffset.x)/(self.scaleIndex)  + _leftRightIndent - 16; //half width hendstrike
        if (scrollView.contentOffset.x > 0 && scrollView.contentOffset.x < scrollView.contentSize.width - 16){
            self.chart.bottomGraph.xHandspikeLeft = newLeftHandsptipeX;
            self.chart.bottomGraph.xHandspikeRight = newLeftHandsptipeX + handspikeDiff;
         }

        [self.chart.bottomGraph setNeedsLayout];
    }
}

- (void)layoutSubviews{
    
    if (!yAxisLabels){
        _bottomChartIndent = 60;
        topChartIndent = 15;
        _leftRightIndent = 60;
        
        int yLabelsCount  = (self.bounds.size.height - _bottomChartIndent - topChartIndent)/50;
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
        yLabel.frame = CGRectMake(10, topChartIndent + ind*(self.bounds.size.height - _bottomChartIndent - topChartIndent)/(yAxisLabels.count - 1),50, 20);
    }
    
    gridScroll.frame = CGRectMake(_leftRightIndent, topChartIndent, self.frame.size.width - 2*_leftRightIndent,
                                  self.frame.size.height - topChartIndent);
    gridScroll.contentSize = CGSizeMake((self.frame.size.width - 2*_leftRightIndent)*self.scaleIndex, self.frame.size.height - topChartIndent - _bottomChartIndent);
    gridArea.frame = CGRectMake(0, 0, (self.frame.size.width - 2*_leftRightIndent)*self.scaleIndex, self.frame.size.height - topChartIndent - _bottomChartIndent);
    //temp dirty hack, TODO chage this!!!
    if (self.scaleIndex != 1){
        [gridScroll setContentOffset:CGPointMake((self.scaleIndex -1)*(self.frame.size.width - 2*_leftRightIndent - _leftShift*self.scaleIndex), 0)];
    }

}

- (void)detectXRange {
    minXVal = self.chart.minXVal;
    maxXVal = self.chart.maxXVal;
}

- (void)drawRect:(CGRect)rect {
    [self detectXRange];
    
    float xFork = maxXVal - minXVal;
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

    
    [gridArea setNeedsDisplay];
    
}

@end
