//
//  NCIChartView.h
//  Tapestry
//
//  Created by Ira on 11/13/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import <UIKit/UIKit.h>
//#import "NCIBottomGraphView.h"
//#import "NCIGraphView.h"

@class NCIMianGraphView;
@class NCIBottomGraphView;

@interface NCIChartView : UIView

//API usage
- (void)addPoint:(NSDate *)date val:(NSString *)value;
- (void)drawChart;
- (void)resetChart;
- (void)setMinArgument:(NSDate *)date;
- (void)setMaxArgument:(NSDate *)date;
@property(nonatomic)bool hasSlider;
@property(nonatomic, strong)NSDate *minRangeDate;
@property(nonatomic, strong)NSDate *maxRangeDate;

//Inside usage
@property (nonatomic, strong)NCIMianGraphView *mainGraph;
@property (nonatomic, strong)NCIBottomGraphView *bottomGraph;
@property (atomic, strong)NSMutableArray *chartData;

@property (nonatomic)int maxXVal;
@property (nonatomic)int minXVal;
@property (nonatomic)int maxYVal;
@property (nonatomic)int minYVal;


@end
