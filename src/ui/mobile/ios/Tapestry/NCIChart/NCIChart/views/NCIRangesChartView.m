//
//  NCIRangesChartView.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/13/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIRangesChartView.h"
#import "NCIGraphView.h"
#import "NCIBottomGraphView.h"
#import "NCIMianGraphView.h"

@interface NCIRangesChartView(){

}

@property (nonatomic)int maxYVal;
@property (nonatomic)int minYVal;
@property (nonatomic)double maxXVal;
@property (nonatomic)double minXVal;

@end

@implementation NCIRangesChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        _topBottomReserve = 5;
        _minXVal = MAXFLOAT;
        _maxXVal = -MAXFLOAT;
        
        _mainGraph = [[NCIMianGraphView alloc] initWithChart:self];
        _mainGraph.backgroundColor = [UIColor whiteColor];
        _mainGraph.gridArea.hasPointSelector = YES;
        [self addSubview:_mainGraph];
        
        _bottomGraph = [[NCIBottomGraphView alloc] initWithChart:self];
        _bottomGraph.backgroundColor = [UIColor whiteColor];
        [self addSubview:_bottomGraph];
        
        _selectedPoint = [[UILabel alloc] initWithFrame:CGRectZero];
        _selectedPoint.font = [UIFont boldSystemFontOfSize:18];
        [self addSubview:_selectedPoint];

    }
    return self;
}

- (void)layoutSubviews{
    float bottomGraphHeight = 110;
    float labelHeight = 20;
    _selectedPoint.frame = CGRectMake(self.bounds.size.width - 320, 0, 300, labelHeight);
    _mainGraph.frame = CGRectMake(0, labelHeight, self.bounds.size.width, self.bounds.size.height - bottomGraphHeight - labelHeight);
    _bottomGraph.frame = CGRectMake(0, self.bounds.size.height - bottomGraphHeight + labelHeight, self.bounds.size.width, bottomGraphHeight);
}

- (void)setMinRangeDate:(NSDate *)minRangeDate{
    NSDate *minDate = (NSDate *)self.chartData[0][0];
    if ([_minRangeDate compare:minDate] == NSOrderedAscending){
         _minRangeDate = minDate;
    } else {
         _minRangeDate = minRangeDate;
    }
}

- (float)getMinValue{
    float diff = _maxYVal - _minYVal;
    if (diff == 0)
        return _minYVal -1;
    return _minYVal - diff*_topBottomReserve/100;
}

- (float)getMaxValue{
    float diff = _maxYVal - _minYVal;
    if (diff == 0)
        return _maxYVal +1;
    return _maxYVal + diff*_topBottomReserve/100;
}

- (float)getMinArgument{
    return _minXVal;
}

- (float)getMaxArgument{
    return _maxXVal;
}

- (void)setMinArgument:(NSDate *)date{
    _minXVal  = [date timeIntervalSince1970];
}

- (void)setMaxArgument:(NSDate *)date{
    _maxXVal = [date timeIntervalSince1970];
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
        
        if ( [_minRangeDate compare:point[0]] <=  NSOrderedSame &&
            [_maxRangeDate compare:point[0]] >= NSOrderedSame ){
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
    if (minYVal == MAXFLOAT)
        return @[[NSNumber numberWithFloat: [self getMinValue]],
                 [NSNumber numberWithFloat: [self getMaxValue]],
                 [NSNumber numberWithLong: 0],
                 [NSNumber numberWithLong: self.chartData.count - 1]];
    float diff = maxYVal - minYVal;
    if (diff == 0){
        maxYVal = maxYVal + 1;
        minYVal = minYVal - 1;
    } else {
        maxYVal = maxYVal + diff*_topBottomReserve/100;
        minYVal = minYVal - diff*_topBottomReserve/100;
    }
    return @[[NSNumber numberWithFloat: minYVal],
             [NSNumber numberWithFloat: maxYVal],
             [NSNumber numberWithLong:  firstDataIndex],
             [NSNumber numberWithLong:  lastChartIndex]];
}

- (void)resetChart{
    [self.chartData removeAllObjects];
    _minXVal = MAXFLOAT;
    _maxXVal = -MAXFLOAT;
    _minYVal = 0;
    _maxYVal = 0;
}

- (void)removeFirstPoint{
    if ( self.chartData[1]){
        _minXVal = [self.chartData[1][0] timeIntervalSince1970];
    }
    [self.chartData removeObjectAtIndex:0];
}

- (void)addPoint:(NSDate *)date val:(NSString *)value{

    int dateSeconds = [date timeIntervalSince1970];
    if (!_minXVal || _minXVal > dateSeconds){
        _minXVal = dateSeconds;
    };
    if (!_maxXVal || _maxXVal < dateSeconds){
        _maxXVal = dateSeconds;
    };
    
    float indexValue = [value integerValue];
    if (!_minYVal || _minYVal > indexValue){
        _minYVal = indexValue;
    };
    if (!_maxYVal || _maxYVal < indexValue){
        _maxYVal = indexValue;
    };
    
    [self.chartData addObject:@[date, value]];
}


- (void)drawChart{
    [_mainGraph setNeedsLayout];
    [_mainGraph setNeedsDisplay];
    [_bottomGraph setNeedsLayout];
    [_bottomGraph setNeedsDisplay];

}

@end
