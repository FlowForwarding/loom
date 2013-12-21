//
//  NCISimpleChartView.h
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface NCISimpleChartView : UIView

@property (atomic, strong)NSMutableArray *chartData;

- (void)addPoint:(NSDate *)date val:(NSString *)value;

- (NSArray *)getBoundaryValues;

@end
