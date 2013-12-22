//
//  NCISimpleGridView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCISimpleGridView.h"

@interface NCISimpleGridView(){
    NCISimpleGraphView *graph;
}

@end

@implementation NCISimpleGridView


- (id)initWithGraph:(NCISimpleGraphView *) ncigraph{
    self = [self initWithFrame:CGRectZero];
    if (self){
        graph = ncigraph;
    }
    return self;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        self.backgroundColor = [UIColor clearColor];
    }
    return self;
}

- (void)drawRect:(CGRect)rect
{
    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    CGContextSetLineWidth(currentContext, 0.3);
    [[UIColor blackColor] setStroke];
    CGFloat dashes[] = { 1, 1 };
    CGContextSetLineDash(currentContext, 0.0,  dashes , 2 );
    
    for (UILabel *yLabel in graph.yAxisLabels){
        CGContextMoveToPoint(currentContext, yLabel.frame.origin.x, yLabel.frame.origin.y);
        CGContextAddLineToPoint(currentContext, self.frame.size.width, yLabel.frame.origin.y);
    }
    
    for (UILabel *xLabel in graph.xAxisLabels){
        CGContextMoveToPoint(currentContext, xLabel.frame.origin.x - graph.xLabelsWidth, xLabel.frame.origin.y);
        CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x - graph.xLabelsWidth, 0);
    }
    CGContextStrokePath(currentContext);
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    if (graph.chart.chartData.count > 0){
        [path moveToPoint:[self pointByServerData:graph.chart.chartData[0]]];
    }
    
    if (graph.chart.chartData.count > 1){
        for (int ind = 1; ind < graph.chart.chartData.count; ind++){
            [path addLineToPoint:[self pointByServerData:graph.chart.chartData[ind]]];
        };
    }
    
    [[UIColor blueColor] setStroke];
    [path setLineWidth:.5];
    [path stroke];
    
}

- (CGPoint)pointByServerData:(NSArray *)data{
    NSDate *date = data[0];
    float yVal = self.frame.size.height - (([data[1] integerValue] - graph.minYVal)*graph.yStep);
    float xVal = [self getXValueByDate: date];
    return CGPointMake(xVal, yVal);
}

- (float)getXValueByDate:(NSDate *)date{
    return ([date timeIntervalSince1970] - graph.minXVal)*graph.xStep;
}

@end
