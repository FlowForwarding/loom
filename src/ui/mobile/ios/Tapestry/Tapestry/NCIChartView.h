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

@end
