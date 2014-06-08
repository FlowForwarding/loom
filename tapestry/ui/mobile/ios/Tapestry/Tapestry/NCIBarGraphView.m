//
//  NCIBarGraphView.m
//  NCIChart
//
//  Created by Ira on 3/11/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIBarGraphView.h"
#import "NCIBarGridView.h"

@implementation NCIBarGraphView

- (void)addSubviews{
    self.grid = [[NCIBarGridView alloc] initWithGraph:self];
    [self addSubview:self.grid];
}

- (void)layoutSubviews{
    [super layoutSubviews];
    self.minXVal = 0;
    self.minYVal = 0;
    self.yStep = self.gridHeigth/(self.maxYVal - self.minYVal);
    self.xStep = self.gridWidth/(self.maxXVal - self.minXVal);
    [self.chart.yAxis redrawLabels:self.gridHeigth min:self.minYVal max:self.maxYVal];
    [self.chart.xAxis redrawLabels:self.gridWidth min:self.minXVal max:self.maxXVal];
    
    [self.grid setNeedsDisplay];
}

@end

