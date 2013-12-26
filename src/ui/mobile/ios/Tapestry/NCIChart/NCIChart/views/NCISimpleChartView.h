//
//  NCISimpleChartView.h
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "NCIChartOptions.h"

@class NCISimpleGraphView;

@interface NCISimpleChartView : UIView

@property (nonatomic, strong)NCISimpleGraphView *graph;
@property (nonatomic, strong)NSMutableArray *chartData;

@property (nonatomic, strong)UILabel *selectedLabel;

@property (nonatomic)bool hasYLabels;
@property (nonatomic)bool hasSelection;
@property (nonatomic)bool incIsFill;

//in persentage
@property (nonatomic)float topBottomGridSpace;


-(id)initWithFrame:(CGRect)frame andOptions:(NSDictionary *)opts;

- (void)addSubviews;
- (void)addPoint:(NSDate *)date val:(NSNumber *)value;
- (NSArray *)getBoundaryValues;
- (void)layoutSelectedPoint;

@end
