//
//  NCITopGridView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCITopGridView.h"
#import "NCITopChartView.h"


@implementation NCITopGridView


- (void)drawGraphLine{
    NSArray *values = [((NCITopChartView *)self.graph.chart) getValsInRanges];
    
    long startIndex = [values[2] integerValue];
    long endIndex = [values[3] integerValue];
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    if (self.graph.chart.chartData.count > startIndex){
        [path moveToPoint:[self.graph pointByServerDataInGrid:self.graph.chart.chartData[startIndex]]];
    }
    
    for (long ind = (startIndex + 1); ind < endIndex; ind++){
        [path addLineToPoint:[self.graph pointByServerDataInGrid:self.graph.chart.chartData[ind]]];
    };

    
    [[UIColor blueColor] setStroke];
    [path setLineWidth:.5];
    [path stroke];
}

@end
