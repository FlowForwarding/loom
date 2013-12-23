//
//  NCISimpleGridView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCISimpleGridView.h"

@interface NCISimpleGridView(){

}

@end

@implementation NCISimpleGridView


- (id)initWithGraph:(NCISimpleGraphView *) ncigraph{
    self = [self initWithFrame:CGRectZero];
    if (self){
        _graph = ncigraph;
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
    //TODO not redraw for top chart every time
    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    CGContextSetLineWidth(currentContext, 0.3);
    [[UIColor blackColor] setStroke];
    CGFloat dashes[] = { 1, 1 };
    CGContextSetLineDash(currentContext, 0.0,  dashes , 2 );
    
    for (UILabel *yLabel in _graph.yAxisLabels){
        CGContextMoveToPoint(currentContext, yLabel.frame.origin.x, yLabel.frame.origin.y);
        CGContextAddLineToPoint(currentContext, self.frame.size.width, yLabel.frame.origin.y);
    }
    
    for (UILabel *xLabel in _graph.xAxisLabels){
        CGContextMoveToPoint(currentContext, xLabel.frame.origin.x - _graph.xLabelsWidth, xLabel.frame.origin.y);
        CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x - _graph.xLabelsWidth, 0);
    }
    CGContextStrokePath(currentContext);
    
    [self drawGraphLine];
}

- (void)drawGraphLine{
    UIBezierPath *path = [UIBezierPath bezierPath];
    if (_graph.chart.chartData.count > 0){
        [path moveToPoint:[_graph pointByServerData:_graph.chart.chartData[0]]];
    }
    
    if (_graph.chart.chartData.count > 1){
        for (int ind = 1; ind < _graph.chart.chartData.count; ind++){
            [path addLineToPoint:[_graph pointByServerData:_graph.chart.chartData[ind]]];
        };
    }
    
    [[UIColor blueColor] setStroke];
    [path setLineWidth:.5];
    [path stroke];
}

@end
