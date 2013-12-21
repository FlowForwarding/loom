//
//  NCISimpleChartView.m
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCISimpleChartView.h"
#import "NCISimpleGraphView.h"

@interface NCISimpleChartView(){
    NCISimpleGraphView *graphView;
}

@end

@implementation NCISimpleChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        graphView = [[NCISimpleGraphView alloc] initWithChart:self];
        [self addSubview:graphView];
        self.backgroundColor = [UIColor clearColor];
        self.chartData = [[NSMutableArray alloc] init];
    }
    return self;
}

- (void)layoutSubviews{
    graphView.frame = self.bounds;
}

- (void)addPoint:(NSDate *)date val:(NSString *)value{
    [self.chartData addObject:@[date, value]];
}

- (NSArray *)getBoundaryValues{
    float minY = MAXFLOAT;
    float maxY = -MAXFLOAT;
    for (NSArray *point in self.chartData){
        float val = [point[1] floatValue];
        if (val < minY){
            minY = val;
        }
        if (val > maxY){
            maxY = val;
        }
    }
    return @[@(minY), @(maxY)];
}

@end
