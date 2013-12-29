//
//  NCITopChartView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCITopChartView.h"
#import "NCITopGraphView.h"
#import "NCIChartView.h"

@interface NCITopChartView(){
}

@end

@implementation NCITopChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}

- (void)addSubviews{
    self.graph = [[NCITopGraphView alloc] initWithChart:self];
    [self addSubview:self.graph];
}

- (NSArray *)getValsInRanges{
    float minYVal = MAXFLOAT;
    float maxYVal = -MAXFLOAT;
    long firstDataIndex = self.chartData.count;
    long lastChartIndex = 0;
    long index;
    for(index = 1; index < self.chartData.count; index++){
        NSArray *point = self.chartData[index];
        if ([point[1][0] isKindOfClass:[NSNull class]]){
            continue;
        }
        NSArray * prevPoint = self.chartData[index - 1];
        if ([prevPoint[1][0] isKindOfClass:[NSNull class]]){
            prevPoint = @[prevPoint[0], point[1]];
        }
        
        NSArray * nextPoint;
        if (self.chartData.count != (index + 1)){
            nextPoint = self.chartData[index + 1];
            if ([nextPoint[1][0] isKindOfClass:[NSNull class]]){
                nextPoint = @[nextPoint[0], point[1]];
            }
        } else {
            nextPoint = point;
        }
        
        float curMax = [prevPoint[1][0] floatValue];
        float curMin = [prevPoint[1][0] floatValue];
        if ([point[1][0] floatValue] > [prevPoint[1][0] floatValue]){
            curMax = [point[1][0] floatValue];
        } else {
            curMin= [point[1][0] floatValue];
        }
        if ( curMax < [nextPoint[1][0] floatValue]){
            curMax = [nextPoint[1][0] floatValue];
        }
        if ( curMin > [nextPoint[1][0] floatValue]){
            curMin = [nextPoint[1][0] floatValue];
        }
        
        if ( self.nciChart.minRangeVal <= [point[0] doubleValue] &&
            ( minYVal == MAXFLOAT || self.nciChart.maxRangeVal  >= [point[0] doubleValue])){
                
            if (firstDataIndex > (index - 1)){
                firstDataIndex = (index - 1);
            }
            if (lastChartIndex < (index + 1)){
                lastChartIndex = (index + 1);
            }
            if (curMin < minYVal)
                minYVal = curMin;
            if (curMax > maxYVal)
                maxYVal = curMax;
        }
    }
    //TODO if to narrow get MAXFLOAT, rethink logic above ^
    if (minYVal == MAXFLOAT)
        return @[@(0), @(1), @(0), @(self.chartData.count)];
    
    float diff = maxYVal - minYVal;
    if (diff == 0){
        maxYVal = maxYVal + 1;
        minYVal = minYVal - 1;
    } else {
        maxYVal = maxYVal + diff*self.topBottomGridSpace/100;
        minYVal = minYVal - diff*self.topBottomGridSpace/100;
    }
    
    if (lastChartIndex < self.chartData.count)
        lastChartIndex = lastChartIndex + 1;

    return @[@(minYVal),
             @(maxYVal),
             @(firstDataIndex),
             @(lastChartIndex)];
}


/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
}
*/

@end
