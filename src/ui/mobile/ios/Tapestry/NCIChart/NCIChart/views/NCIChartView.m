//
//  NCIChartView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCIChartView.h"
#import "NCITopChartView.h"
#import "NCITopGridView.h"
#import "NCIBtmChartView.h"

@interface NCIChartView(){
    
    float btmChartHeigth;
    float chartsSpace;
}

@end

@implementation NCIChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad){
            btmChartHeigth =  90;
            chartsSpace = 30;
        } else {
            btmChartHeigth =  60;
            chartsSpace = 10;
        }
        
    }
    return self;
}

- (void)addSubviews{
    _topChart = [[NCITopChartView alloc] initWithFrame:CGRectZero];
    _topChart.chartData = self.chartData;
    _topChart.nciChart = self;
    _topChart.nciHasSelection = YES;
    _btmChart = [[NCIBtmChartView alloc] initWithFrame:CGRectZero];
    _btmChart.chartData = self.chartData;
    _btmChart.hasYLabels = NO;
    _btmChart.nciChart = self;
    [self addSubview:_topChart];
    [self addSubview:_btmChart];
}

-(void)drawChart{
    [_topChart setNeedsLayout];
    [_btmChart setNeedsLayout];

}

- (void)resetChart{
    [self.chartData removeAllObjects];
}

- (void)layoutSubviews{
    _topChart.frame = CGRectMake(0, 0, self.frame.size.width, self.frame.size.height - btmChartHeigth - chartsSpace);
    _btmChart.frame = CGRectMake(0, self.frame.size.height - btmChartHeigth, self.frame.size.width, btmChartHeigth);
}

- (float)getScaleIndex{
    if (!_minRangeDate || !_maxRangeDate)
        return 1;
    float rangeDiff = [_maxRangeDate timeIntervalSince1970] - [_minRangeDate timeIntervalSince1970];
    if (rangeDiff == 0){
        return  1;
    } else {
        return ([[self.chartData lastObject][0] timeIntervalSince1970] - [self.chartData[0][0] timeIntervalSince1970])/rangeDiff;
    }
}

-(float)getTimePeriod{
    return [[self.chartData lastObject][0] timeIntervalSince1970] - [self.chartData[0][0] timeIntervalSince1970];
}

-(float)getRangesPeriod{
    return [self.maxRangeDate timeIntervalSince1970] - [self.minRangeDate timeIntervalSince1970];
}

@end
