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
    int bottomChartIndent;
    int topChartIndent;
    int xLabelShift;
    NSDateFormatter* dateFormatter;
}

@end

@implementation NCIGraphView

- (id)initWithChart: (NCIChartView *)chartHolder{
    self = [self initWithFrame:CGRectZero];
    if (self){
        self.chart = chartHolder;
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
        
        bottomChartIndent = 80;
        topChartIndent = 80;
        xLabelShift = 75;
        
        int yLabelsCount  = 5;
        yAxisLabels = [[NSMutableArray alloc] initWithCapacity:yLabelsCount];
        
        
        int ind = 0;
        for (ind = 0; ind< yLabelsCount; ind++){
            UILabel *yLabel = [[UILabel alloc] initWithFrame:CGRectZero];
            yLabel.backgroundColor = [UIColor clearColor];
            [yAxisLabels addObject:yLabel];
            [self addSubview:yLabel];
        };
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
    int ind;
    for (ind = 0; ind< yAxisLabels.count; ind++){
        UILabel *yLabel = yAxisLabels[ind];
        yLabel.frame = CGRectMake(10, topChartIndent + ind*(self.bounds.size.height - bottomChartIndent - topChartIndent)/(yAxisLabels.count - 1),50, 20);
    }
    for (ind = 0; ind< xAxisLabels.count; ind++){
        UILabel *xLabel = xAxisLabels[ind];
        xLabel.frame = CGRectMake(topChartIndent - xLabelShift + ind*(self.bounds.size.width - topChartIndent*2)/(xAxisLabels.count - 1),
                                  self.bounds.size.height - bottomChartIndent + 25,
                                  150, 20);
    };
}

- (void)drawRect:(CGRect)rect {
    
    float xFork = self.chart.maxXVal - self.chart.minXVal;
    float xStep = (self.bounds.size.width - topChartIndent*2)/xFork;
    
    float yFork = self.chart.maxYVal - self.chart.minYVal;
    float yStep = (self.bounds.size.height - bottomChartIndent - topChartIndent)/yFork;
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    [path setLineWidth:.5];
    if (self.chart.chartData.count > 0){
        NSDate *date = self.chart.chartData[0][0];
        int yVal = self.frame.size.height - (bottomChartIndent + ([self.chart.chartData[0][1] integerValue] - self.chart.minYVal)*yStep);
        int xVal = topChartIndent + ([date timeIntervalSince1970] - self.chart.minXVal)*xStep;
        [path moveToPoint:CGPointMake(xVal, yVal)];
    }
    
    int ind;
    for (ind = 1; ind < self.chart.chartData.count; ind++){
        NSDate *date = self.chart.chartData[ind][0];
        int yVal = self.frame.size.height - (bottomChartIndent + ([self.chart.chartData[ind][1] integerValue] - self.chart.minYVal)*yStep);
        int xVal = topChartIndent + ([date timeIntervalSince1970] - self.chart.minXVal)*xStep;
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
        for (ind = 0; ind< yAxisLabels.count; ind++){
            UILabel *yLabel = yAxisLabels[ind];
            yLabel.text = [NSString stringWithFormat:@"%.1f", self.chart.maxYVal - ind * yFork/(yAxisLabels.count - 1)];
            CGContextMoveToPoint(currentContext, yLabel.frame.origin.x + topChartIndent/2, yLabel.frame.origin.y);
            CGContextAddLineToPoint(currentContext, self.frame.size.width - topChartIndent/2, yLabel.frame.origin.y);
            CGContextStrokePath(currentContext);
        };
    };
    
    if (self.chart.maxXVal && self.chart.minXVal){
        for (ind = 0; ind< xAxisLabels.count; ind++){
            UILabel *xLabel = xAxisLabels[ind];
            NSDate *date = [NSDate dateWithTimeIntervalSince1970:(self.chart.minXVal + ind * xFork/(xAxisLabels.count - 1))];
            xLabel.text = [NSString stringWithFormat:@"%@", [dateFormatter stringFromDate: date]];
            CGContextMoveToPoint(currentContext, xLabel.frame.origin.x + xLabelShift , xLabel.frame.origin.y );
            CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x + xLabelShift, topChartIndent/2);
            CGContextStrokePath(currentContext);
        };
    };
    
}

@end
