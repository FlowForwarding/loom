//
//  NCIGridAreaView.m
//  Tapestry
//
//  Created by Ira on 11/20/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGridAreaView.h"

@interface NCIGridAreaView(){
    NCIChartView *chart;
    NSMutableArray *xAxisLabels;
    NSDateFormatter* dateFormatter;
}

@end

@implementation NCIGridAreaView

-(id)initWithChart:(NCIChartView *)generalChart{
    self = [self initWithFrame:CGRectZero];
    if (self){
        chart = generalChart;
        self.backgroundColor = [UIColor clearColor];
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
            dateFormatter = [[NSDateFormatter alloc] init];
            [dateFormatter setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
            [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm"];
        };

    }
    return self;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}

- (void)layoutSubviews{
    int ind;
    for (ind = 0; ind< xAxisLabels.count; ind++){
        UILabel *xLabel = xAxisLabels[ind];
        xLabel.frame = CGRectMake(ind*(self.bounds.size.width)/(xAxisLabels.count - 1),
                                  self.bounds.size.height  + 15,
                                  150, 20);
    };
}


- (void)drawRect:(CGRect)rect
{
    float xFork = chart.maxXVal - chart.minXVal;
    float xStep = self.bounds.size.width/xFork;
    
    float yFork = chart.maxYVal - chart.minYVal;
    float yStep = self.bounds.size.height/yFork;
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    [path setLineWidth:.5];
    if (chart.chartData.count > 0){
        NSDate *date = chart.chartData[0][0];
        int yVal = self.frame.size.height - (([chart.chartData[0][1] integerValue] - chart.minYVal)*yStep);
        int xVal = ([date timeIntervalSince1970] - chart.minXVal)*xStep;
        [path moveToPoint:CGPointMake(xVal, yVal)];
    }
    
    int ind;
    for (ind = 1; ind < chart.chartData.count; ind++){
        NSDate *date = chart.chartData[ind][0];
        int yVal = self.frame.size.height - (([chart.chartData[ind][1] integerValue] - chart.minYVal)*yStep);
        int xVal =  ([date timeIntervalSince1970] - chart.minXVal)*xStep;
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
    
    if (chart.maxXVal && chart.minXVal){
        for (ind = 0; ind< xAxisLabels.count; ind++){
            UILabel *xLabel = xAxisLabels[ind];
            NSDate *date = [NSDate dateWithTimeIntervalSince1970:(chart.minXVal + ind * xFork/(xAxisLabels.count - 1))];
            xLabel.text = [NSString stringWithFormat:@"%@", [dateFormatter stringFromDate: date]];
           // if (self.hasGrid || ind == 0){
                CGContextMoveToPoint(currentContext, xLabel.frame.origin.x, xLabel.frame.origin.y );
                CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x, 0);
                CGContextStrokePath(currentContext);
           // }
        };
    };
    
}


@end
