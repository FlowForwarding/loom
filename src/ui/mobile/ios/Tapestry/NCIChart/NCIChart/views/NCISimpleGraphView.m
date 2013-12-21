//
//  NCISimpleGraphView.m
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCISimpleGraphView.h"
#import "NCISimpleChartView.h"

@interface NCISimpleGraphView(){
    NSMutableArray *yAxisLabels;
    NSMutableArray *xAxisLabels;
    float xLabelsDistance;
    float yLabelsDistance;
    float xLabelsWidth;
    float yLabelsHeigth;
    
    NCISimpleChartView *chart;
    
    float gridHeigth;
    float gridWidth;
    
    double yStep;
    double xStep;
    double minYVal;
    double maxYVal;
    double minXVal;
    double maxXVal;
    
}

@end

@implementation NCISimpleGraphView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        xLabelsDistance = 200;
        yLabelsDistance = 50;
        xLabelsWidth = 100;
        yLabelsHeigth = 20;
        yAxisLabels = [[NSMutableArray alloc] init];
        xAxisLabels = [[NSMutableArray alloc] init];
        self.backgroundColor = [UIColor clearColor];
    }
    return self;
}

- (id)initWithChart: (NCISimpleChartView *)chartHolder{
    self = [self initWithFrame:CGRectZero];
    if (self){
        chart = chartHolder;
    }
    return  self;
}

- (void)layoutSubviews{
    for (UILabel *label in yAxisLabels){
        [label removeFromSuperview];
    }
    for (UILabel *label in xAxisLabels){
        [label removeFromSuperview];
    }
    [yAxisLabels removeAllObjects];
    [xAxisLabels removeAllObjects];
    
    gridHeigth = self.frame.size.height- yLabelsHeigth;
    gridWidth = self.frame.size.width - xLabelsWidth;
    if (chart.chartData.count > 0){
        NSArray *yVals = [chart getBoundaryValues];
        minYVal = [yVals[0] floatValue];
        maxYVal = [yVals[1] floatValue];
        yStep = gridHeigth/(maxYVal - minYVal);
        minXVal = [chart.chartData[0][0] timeIntervalSince1970];
        maxXVal = [[chart.chartData lastObject][0] timeIntervalSince1970];
        xStep = gridWidth/(maxXVal - minXVal);
        
        for(int i = 0; i<= gridHeigth/yLabelsDistance; i++){
            UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(0, self.frame.size.height - i*yLabelsDistance - yLabelsHeigth, xLabelsWidth, 20)];
            label.text = [@([self getDateByY: (yLabelsHeigth + yLabelsDistance *i)]) description];
            [yAxisLabels addObject:label];
            [self addSubview:label];
        }
        
        for(int i = 0; i<= gridWidth/xLabelsDistance; i++){
            UILabel *label = [[UILabel alloc] initWithFrame:
                              CGRectMake(xLabelsWidth + xLabelsDistance *i,
                                         self.frame.size.height -yLabelsHeigth, xLabelsDistance,
                                         yLabelsHeigth)];
            label.text = [[self getDateByX: (xLabelsWidth + xLabelsDistance *i)] description];
            [xAxisLabels addObject:label];
            [self addSubview:label];
        }
    }
    [self setNeedsDisplay];
}

- (void)drawRect:(CGRect)rect
{
    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    CGContextSetLineWidth(currentContext, 0.3);
    [[UIColor blackColor] setStroke];
    CGFloat dashes[] = { 1, 1 };
    CGContextSetLineDash(currentContext, 0.0,  dashes , 2 );
    
    for (UILabel *yLabel in yAxisLabels){
        CGContextMoveToPoint(currentContext, yLabel.frame.origin.x + xLabelsWidth, yLabel.frame.origin.y);
        CGContextAddLineToPoint(currentContext, self.frame.size.width, yLabel.frame.origin.y);
    }
    
    for (UILabel *xLabel in xAxisLabels){
        CGContextMoveToPoint(currentContext, xLabel.frame.origin.x, xLabel.frame.origin.y);
        CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x, 0);
    }
    CGContextStrokePath(currentContext);
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    if (chart.chartData.count > 0){
        [path moveToPoint:[self pointByServerData:chart.chartData[0]]];
    }

    if (chart.chartData.count > 1){
        for (int ind = 1; ind < chart.chartData.count; ind++){
            [path addLineToPoint:[self pointByServerData:chart.chartData[ind]]];
        };
    }

    [[UIColor blueColor] setStroke];
    [path setLineWidth:.5];
    [path stroke];

}

- (CGPoint)pointByServerData:(NSArray *)data{
    NSDate *date = data[0];
    float yVal = self.frame.size.height - (([data[1] integerValue] - minYVal)*yStep) - yLabelsHeigth;
    float xVal = [self getXValueByDate: date];
    return CGPointMake(xVal, yVal);
}

- (float)getXValueByDate:(NSDate *)date{
    return xLabelsWidth + ([date timeIntervalSince1970] - minXVal)*xStep;
}

- (NSDate *)getDateByX:(float) pointX{
    return [NSDate dateWithTimeIntervalSince1970:(minXVal + (pointX - xLabelsWidth)/xStep)];
}

- (float )getDateByY:(float) pointY{
    return minYVal + (pointY - yLabelsHeigth)/yStep;
}



@end
