//
//  NCISimpleChartView.h
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import <UIKit/UIKit.h>

@class NCISimpleGraphView;

@interface NCISimpleChartView : UIView

@property (nonatomic, strong)NCISimpleGraphView *graph;
@property (nonatomic, strong)NSMutableArray *chartData;

@property (nonatomic)bool hasSelection;
@property (nonatomic, strong)UILabel *selectedLabel;
@property (nonatomic)bool hasYLabels;

//in persentage
@property (nonatomic)float topBottomGridSpace;

- (void)addSubviews;

- (void)addPoint:(NSDate *)date val:(NSString *)value;

- (NSArray *)getBoundaryValues;

- (void)layoutSelectedPoint;

@end
