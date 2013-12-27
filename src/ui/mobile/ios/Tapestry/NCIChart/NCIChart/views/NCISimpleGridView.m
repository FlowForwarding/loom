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
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    [self.graph.chart.nciLineColor setStroke];
    [path setLineWidth: self.graph.chart.nciLineWidth];
    [[self.graph.chart.nciLineColor colorWithAlphaComponent:0.1] setFill];
    [self drawGraphLine:path for:[self getFirstLast]];

    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    CGContextSetLineWidth(currentContext, 0.3);
    [[UIColor blackColor] setStroke];
    CGFloat dashes[] = { 1, 1 };
    CGContextSetLineDash(currentContext, 0.0,  dashes , 2 );
    
    for (UILabel *yLabel in _graph.yAxisLabels){
        CGContextMoveToPoint(currentContext, yLabel.frame.origin.x, yLabel.frame.origin.y + self.graph.yLabelShift);
        CGContextAddLineToPoint(currentContext, self.frame.size.width, yLabel.frame.origin.y + self.graph.yLabelShift);
    }
    
    for (UILabel *xLabel in _graph.xAxisLabels){
        CGContextMoveToPoint(currentContext, xLabel.frame.origin.x - _graph.xLabelsWidth + self.graph.chart.nciXLabelsDistance/2, xLabel.frame.origin.y);
        CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x - _graph.xLabelsWidth + self.graph.chart.nciXLabelsDistance/2, 0);
    }
    CGContextStrokePath(currentContext);
    
}

- (NSArray *)getFirstLast{
    return @[@(0), @(self.graph.chart.chartData.count)];
}

- (void)drawGraphLine:(UIBezierPath *)path for:(NSArray *)firstLast{
    
    long lastMoveInd = [firstLast[0] integerValue] - 1;
    long firstInd = [firstLast[0] integerValue];
    if (_graph.chart.chartData.count >  1){
        for (long ind = firstInd; ind < [firstLast[1] integerValue]; ind++){
            NSArray *point = _graph.chart.chartData[ind];
            if ([point[1] isKindOfClass:[NSNull class]] ){
                if (lastMoveInd != (ind -1) && self.graph.chart.nciIsFill){
                    [path addLineToPoint:CGPointMake( path.currentPoint.x, self.frame.size.height)];
                    [path moveToPoint: [_graph pointByServerDataInGrid:point]];
                }
                lastMoveInd = ind;
            } else {
                CGPoint pointP = [_graph pointByServerDataInGrid:point];
                if (lastMoveInd == (ind -1)){
                    if (self.graph.chart.nciIsFill){
                        [path moveToPoint: CGPointMake(pointP.x, self.frame.size.height)];
                    } else {
                        [path moveToPoint:[_graph pointByServerDataInGrid:point]];
                    }
                }
                [path addLineToPoint:[_graph pointByServerDataInGrid:point]];
            }
        };
    }
    
    if (self.graph.chart.nciIsFill && !path.empty){
        [path addLineToPoint:CGPointMake( path.currentPoint.x, self.frame.size.height)];
        [path fill];
        [path closePath];
    }
    [path stroke];
}

@end
