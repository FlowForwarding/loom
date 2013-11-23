//
//  NCIChartView.h
//  Tapestry
//
//  Created by Ira on 11/13/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import <UIKit/UIKit.h>
@class NCIMianGraphView;
@class NCIBottomGraphView;

@interface NCIChartView : UIView

//API usage
- (void)addPoint:(NSDate *)date val:(NSString *)value;
- (void)drawChart;
- (void)resetChart;

- (void)setMinArgument:(NSDate *)date;
- (void)setMaxArgument:(NSDate *)date;

@property(nonatomic, strong)NSDate *minRangeDate;
@property(nonatomic, strong)NSDate *maxRangeDate;

//top and bottom reserve for grph Y values in persentage, default 5%
@property(nonatomic)float topBottomReserve;
@property(nonatomic)bool hasSlider; 

//Inside usage
- (float)getMinValue;
- (float)getMaxValue;
@property (nonatomic)float maxXVal;
@property (nonatomic)float minXVal;
//TODO make a class for point
@property (nonatomic, strong)UILabel *selectedPoint;
@property (nonatomic, strong)NCIMianGraphView *mainGraph;
@property (nonatomic, strong)NCIBottomGraphView *bottomGraph;
@property (atomic, strong)NSMutableArray *chartData;



@end
