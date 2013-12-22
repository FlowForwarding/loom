//
//  NCIChartView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCIChartView.h"
#import "NCISimpleChartView.h"
#import "NCIBtmChartView.h"

@interface NCIChartView(){
    NCISimpleChartView *topChart;
    NCIBtmChartView *btmChart;
    
    float btmChartHeigth;
}

@end

@implementation NCIChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        btmChartHeigth =  200;
    }
    return self;
}

- (void)addSubviews{
    topChart = [[NCISimpleChartView alloc] initWithFrame:CGRectZero];
    topChart.backgroundColor = [UIColor grayColor];
    topChart.chartData = self.chartData;
    btmChart = [[NCIBtmChartView alloc] initWithFrame:CGRectZero];
    btmChart.chartData = self.chartData;
    [self addSubview:topChart];
    [self addSubview:btmChart];
}

- (void)layoutSubviews{
    topChart.frame = CGRectMake(0, 0, self.frame.size.width, self.frame.size.height - btmChartHeigth);
    btmChart.frame = CGRectMake(0, self.frame.size.height - btmChartHeigth, self.frame.size.width, btmChartHeigth);
}

@end
