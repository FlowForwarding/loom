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
    for(index = 1; index < (self.chartData.count - 1); index++){
        NSArray * prevPoint = self.chartData[index - 1];
        NSArray *point = self.chartData[index];
        NSArray * nextPoint = self.chartData[index + 1];
        
        float curMax = [prevPoint[1] floatValue];
        float curMin = [prevPoint[1] floatValue];
        if ([point[1] floatValue] > [prevPoint[1] floatValue]){
            curMax = [point[1] floatValue];
        } else {
            curMin= [point[1] floatValue];
        }
        if ( curMax < [nextPoint[1] floatValue]){
            curMax = [nextPoint[1] floatValue];
        }
        if ( curMin > [nextPoint[1] floatValue]){
            curMin = [nextPoint[1] floatValue];
        }
        
        if ( [self.nciChart.minRangeDate compare:point[0]] <=  NSOrderedSame &&
            [self.nciChart.maxRangeDate compare:point[0]] >= NSOrderedSame ){
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

    return @[@(minYVal),
             @(maxYVal),
             @(firstDataIndex),
             @(lastChartIndex + 1)];
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
