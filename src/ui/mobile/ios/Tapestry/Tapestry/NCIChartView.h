//
//  NCIChartView.h
//  Tapestry
//
//  Created by Ira on 11/13/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface NCIChartView : UIView

- (void)addPoint:(NSDate *)date val:(NSString *)value;
- (void)drawChart;
- (void)resetChart;

@property (atomic, strong)NSMutableArray *chartData;
@property (nonatomic)int maxXVal;
@property (nonatomic)int minXVal;
@property (nonatomic)int maxYVal;
@property (nonatomic)int minYVal;

@end
