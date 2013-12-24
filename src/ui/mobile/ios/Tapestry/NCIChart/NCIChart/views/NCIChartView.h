//
//  NCIChartView.h
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "NCISimpleChartView.h"
@class NCITopChartView;
@class NCIBtmChartView;

@interface NCIChartView : NCISimpleChartView

@property(nonatomic, strong) NCITopChartView *topChart;
@property(nonatomic, strong) NCIBtmChartView *btmChart;

@property(nonatomic, strong)NSDate *minRangeDate;
@property(nonatomic, strong)NSDate *maxRangeDate;

-(float)getScaleIndex;
-(float)getTimePeriod;
-(float)getRangesPeriod;
-(void)resetChart;
-(void)drawChart;

//callbacks
@property (nonatomic, copy) void (^rangesMoved)(void);

@end
