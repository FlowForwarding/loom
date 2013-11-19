//
//  NCIGraphView.h
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "NCIChartView.h"

@interface NCIGraphView : UIView

- (id)initWithChart: (NCIChartView *)chartHolder;
@property(nonatomic, strong) NCIChartView* chart;

@property(nonatomic)bool hasGrid;
@property(nonatomic)bool hasYLabels;

@end
