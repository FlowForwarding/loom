//
//  NCIBottomGraphView.m
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIBottomGraphView.h"

@implementation NCIBottomGraphView

- (id)initWithChart: (NCIChartView *)chartHolder{
    self = [super initWithChart:chartHolder];
    if (self){
        self.chart = chartHolder;
        self.hasGrid = NO;
        self.hasYLabels = NO;
    }
    return self;
}


@end
