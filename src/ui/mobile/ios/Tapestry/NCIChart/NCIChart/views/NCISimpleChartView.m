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
   
}

@end

@implementation NCISimpleChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        self.backgroundColor = [UIColor clearColor];
        self.chartData = [[NSMutableArray alloc] init];
        [self addSubviews];
    }
    return self;
}

- (void)addSubviews{
    _graph = [[NCISimpleGraphView alloc] initWithChart:self];
    [self addSubview:_graph];
}

- (void)layoutSubviews{
    _graph.frame = self.bounds;
    [_graph layoutSubviews];
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
