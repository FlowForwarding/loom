//
//  NCIChartView.m
//  Tapestry
//
//  Created by Ira on 11/13/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIChartView.h"

@interface NCIChartView(){
    int maxXVal;
    int minXVal;
    
    int maxYVal;
    int minYVal;
    
    NSDateFormatter* dateFormatter;
}
@property (atomic, strong)NSMutableArray *chartData;
@end

@implementation NCIChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        self.chartData = [[NSMutableArray alloc] init];
        dateFormatter = [[NSDateFormatter alloc] init];
        [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm:ss"];
    }
    return self;
}

- (void)addPoint:(NSDate *)date val:(NSString *)value{
    NSLog(@"%@", date);
    NSLog(@"%i", [value integerValue]);
    int dateSeconds = [date timeIntervalSince1970];
    if (!minXVal || minXVal > dateSeconds){
        minXVal = dateSeconds;
    };
    if (!maxXVal || maxXVal < dateSeconds){
        maxXVal = dateSeconds;
    };
    
    int indexValue = [value integerValue];
    if (!minYVal || minYVal > indexValue){
        minYVal = indexValue;
    };
    if (!maxYVal || maxYVal < indexValue){
        maxYVal = indexValue;
    };
    
    [self.chartData addObject:@[date, value]];
}

- (void)drawChart{
    [self setNeedsDisplay];
}

- (void)drawRect:(CGRect)rect {
    

    int xFork = maxXVal - minXVal;
    int chartIndent = 50;
    
    float xStep = (self.bounds.size.width - chartIndent*2)/xFork;
    
    int yFork = maxYVal - minYVal;
    float yStep = (self.bounds.size.height - chartIndent*2)/yFork;
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    [path setLineWidth:.5];
    [path moveToPoint:CGPointMake(chartIndent, chartIndent)];
    
    int ind;
    for (ind =0; ind < self.chartData.count; ind++){
        NSDate *date = self.chartData[ind][0];
        int yVal = self.frame.size.height - (chartIndent + ([self.chartData[ind][1] integerValue] - minYVal)*yStep);
        int xVal = chartIndent + ([date timeIntervalSince1970] - minXVal)*xStep;
        [path addLineToPoint:CGPointMake(xVal, yVal)];
        
        UILabel *yLabel = [[UILabel alloc] initWithFrame:CGRectMake(10, yVal, 50, 20)];
        yLabel.text = self.chartData[ind][1];
        [self addSubview:yLabel];
        
        UILabel *xLabel = [[UILabel alloc] initWithFrame:CGRectMake(xVal - 30, self.frame.size.height + 15, 150, 20)];
        xLabel.font = [UIFont systemFontOfSize:14];
        xLabel.layer.transform = CATransform3DMakeRotation(1, 0, 0, 1);
        
        NSString *dateString = [dateFormatter stringFromDate:date];
        xLabel.text = dateString;
        [self addSubview:xLabel];
        
    };
    
    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    CGContextSetLineCap(currentContext, kCGLineCapRound);
    CGContextSetLineJoin(currentContext, kCGLineJoinRound);
    CGContextBeginPath(currentContext);
    CGContextAddPath(currentContext, path.CGPath);
    CGContextDrawPath(currentContext, kCGPathStroke);
}


@end
