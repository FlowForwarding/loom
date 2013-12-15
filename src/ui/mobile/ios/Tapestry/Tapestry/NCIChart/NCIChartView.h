//
//  NCIChartView.h
//  Tapestry
//
//  Created by Infoblox Inc on 11/13/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import <UIKit/UIKit.h>
@class NCIMianGraphView;
@class NCIBottomGraphView;

@interface NCIChartView : UIView

//API usage
- (void)addPoint:(NSDate *)date val:(NSString *)value;
- (void)removeFirstPoint;
- (void)drawChart;
- (void)resetChart;

- (void)setMinArgument:(NSDate *)date;
- (void)setMaxArgument:(NSDate *)date;

@property(nonatomic, strong)NSDate *minRangeDate;
@property(nonatomic, strong)NSDate *maxRangeDate;

//top and bottom reserve for grph Y values in persentage, default 5%
@property(nonatomic)float topBottomReserve;
@property(nonatomic)bool hasRangeSelector; 

//Inside usage
//All date values are in convertion to timeIntervalSince1970
- (float)getMinValue;
- (float)getMaxValue;
- (float)getMinArgument;
- (float)getMaxArgument;
- (NSArray *)getValsInRanges;

//TODO make a class for point
@property (nonatomic, strong)UILabel *selectedPoint;
@property (nonatomic, strong)NCIMianGraphView *mainGraph;
@property (nonatomic, strong)NCIBottomGraphView *bottomGraph;
@property (atomic, strong)NSMutableArray *chartData;



@end
