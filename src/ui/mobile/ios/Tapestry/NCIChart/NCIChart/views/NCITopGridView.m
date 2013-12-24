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


- (NSArray *)getFirstLast{
    NSArray *values = [((NCITopChartView *)self.graph.chart) getValsInRanges];
    return @[values[2], values[3]];
}



@end
