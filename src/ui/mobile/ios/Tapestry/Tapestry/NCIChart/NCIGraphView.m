//
//  NCIGraphView.m
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGraphView.h"

@interface NCIGraphView(){
    NSMutableArray *yAxisLabels;
    NSMutableArray *xAxisLabels;
    float topChartIndent;
    NSDateFormatter* dateFormatter;
}

@end

@implementation NCIGraphView

- (id)initWithChart: (NCIChartView *)chartHolder{
    self = [self initWithFrame:CGRectZero];
    if (self){
        self.chart = chartHolder;
        self.hasGrid = YES;
        self.hasYLabels = YES;
    }
    return self;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        dateFormatter = [[NSDateFormatter alloc] init];
        [dateFormatter setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
        [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm:ss"];
        
        
        int ind = 0;
        int xLabelsCount  = 4;
        xAxisLabels = [[NSMutableArray alloc] initWithCapacity:xLabelsCount];
        for (ind = 0; ind< xLabelsCount; ind++){
            UILabel *xLabel = [[UILabel alloc] initWithFrame: CGRectZero];
            xLabel.font = [UIFont italicSystemFontOfSize:14];
            // CATransform3D transform = CATransform3DMakeRotation(M_PI/3, 0, 0, 1);
            // xLabel.layer.transform = transform;
            
            [xAxisLabels addObject:xLabel];
            [self addSubview:xLabel];
        };
        
    }
    return self;
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
    for (ind = 0; ind< xAxisLabels.count; ind++){
        UILabel *xLabel = xAxisLabels[ind];
        xLabel.frame = CGRectMake(_leftRightIndent + ind*(self.bounds.size.width - _leftRightIndent*2)/(xAxisLabels.count - 1),
                                  self.bounds.size.height - _bottomChartIndent + 15,
                                  150, 20);
    };
}

- (void)drawRect:(CGRect)rect {
    
    float xFork = self.chart.maxXVal - self.chart.minXVal;
    float xStep = (self.bounds.size.width - _leftRightIndent*2)/xFork;
    
    float yFork = self.chart.maxYVal - self.chart.minYVal;
    float yStep = (self.bounds.size.height - _bottomChartIndent - topChartIndent)/yFork;
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    [path setLineWidth:.5];
    if (self.chart.chartData.count > 0){
        NSDate *date = self.chart.chartData[0][0];
        int yVal = self.frame.size.height - (_bottomChartIndent + ([self.chart.chartData[0][1] integerValue] - self.chart.minYVal)*yStep);
        int xVal = _leftRightIndent + ([date timeIntervalSince1970] - self.chart.minXVal)*xStep;
        [path moveToPoint:CGPointMake(xVal, yVal)];
    }
    
    int ind;
    for (ind = 1; ind < self.chart.chartData.count; ind++){
        NSDate *date = self.chart.chartData[ind][0];
        int yVal = self.frame.size.height - (_bottomChartIndent + ([self.chart.chartData[ind][1] integerValue] - self.chart.minYVal)*yStep);
        int xVal = _leftRightIndent + ([date timeIntervalSince1970] - self.chart.minXVal)*xStep;
        [path addLineToPoint:CGPointMake(xVal, yVal)];
        
    };
    
    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    CGContextSetLineCap(currentContext, kCGLineCapRound);
    CGContextSetLineWidth(currentContext, 0.5);
    CGContextSetLineJoin(currentContext, kCGLineJoinRound);
    CGContextBeginPath(currentContext);
    [[UIColor blueColor] setStroke];
    CGContextAddPath(currentContext, path.CGPath);
    CGContextDrawPath(currentContext, kCGPathStroke);
    [[UIColor blackColor] setStroke];
    
    
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
    
    if (self.chart.maxXVal && self.chart.minXVal){
        for (ind = 0; ind< xAxisLabels.count; ind++){
            UILabel *xLabel = xAxisLabels[ind];
            NSDate *date = [NSDate dateWithTimeIntervalSince1970:(self.chart.minXVal + ind * xFork/(xAxisLabels.count - 1))];
            xLabel.text = [NSString stringWithFormat:@"%@", [dateFormatter stringFromDate: date]];
            if (self.hasGrid || ind == 0){
                CGContextMoveToPoint(currentContext, xLabel.frame.origin.x, xLabel.frame.origin.y );
                CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x, topChartIndent/2);
                CGContextStrokePath(currentContext);
            }
        };
    };
    
}

@end
