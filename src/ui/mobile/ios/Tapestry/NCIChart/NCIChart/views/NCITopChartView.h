//
//  NCITopChartView.h
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCISimpleChartView.h"

@class NCIChartView;

@interface NCITopChartView : NCISimpleChartView

@property(nonatomic, strong) NCIChartView* nciChart;

- (NSArray *)getValsInRanges;

@end
