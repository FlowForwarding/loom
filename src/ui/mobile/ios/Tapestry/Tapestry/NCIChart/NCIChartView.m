//
//  NCIChartView.m
//  Tapestry
//
//  Created by Ira on 11/13/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIChartView.h"
#import "NCIGraphView.h"
#import "NCIBottomGraphView.h"
#import "NCIMianGraphView.h"

@interface NCIChartView(){

}

@property (nonatomic)int maxYVal;
@property (nonatomic)int minYVal;
@property (nonatomic)double maxXVal;
@property (nonatomic)double minXVal;

@end

@implementation NCIChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        _hasRangeSelector = YES;
        _topBottomReserve = 5;
        _minXVal = MAXFLOAT;
        _maxXVal = -MAXFLOAT;
        
        self.chartData = [[NSMutableArray alloc] init];
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
    if (_hasRangeSelector){
        _mainGraph.frame = CGRectMake(0, labelHeight, self.bounds.size.width, self.bounds.size.height - bottomGraphHeight - labelHeight);
        _bottomGraph.frame = CGRectMake(0, self.bounds.size.height - bottomGraphHeight + labelHeight, self.bounds.size.width, bottomGraphHeight);
    } else {
        _mainGraph.frame = self.bounds;
    }
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
        return _maxYVal -1;
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
    float minXVal = MAXFLOAT;
    float maxXVal = -MAXFLOAT;
    long firstDataIndex = self.chartData.count;
    long lastChartIndex = 0;
    long index;
    for(index = 0; index < self.chartData.count; index++){
        NSArray *prevPoint;
        if (index == 0) {
            prevPoint = self.chartData[index];
        } else {
            prevPoint = self.chartData[index - 1];
        };
        NSArray *point = self.chartData[index];
        NSArray *nextPoint;
        if (index == (self.chartData.count - 1)){
            nextPoint = self.chartData[index];
        } else {
            nextPoint = self.chartData[index + 1];
        }
        float cur = [point[1] floatValue];
        if ( [_minRangeDate compare:prevPoint[0]] <  NSOrderedDescending ||
            [_maxRangeDate compare:nextPoint[0]] > NSOrderedAscending ){
            if (firstDataIndex > index){
                firstDataIndex = index;
            }
            if (lastChartIndex < index){
                lastChartIndex = index;
            }
            if (cur < minXVal)
                minXVal = cur;
            if (cur> maxXVal)
                maxXVal = cur;
        }
    }
    if (minXVal == MAXFLOAT)
        return @[[NSNumber numberWithFloat: [self getMinValue]],
                 [NSNumber numberWithFloat: [self getMaxValue]],
                 [NSNumber numberWithLong: 0],
                 [NSNumber numberWithLong: self.chartData.count - 1]];
    float diff = maxXVal - minXVal;
    if (diff == 0){
        maxXVal = maxXVal + 1;
        minXVal = minXVal - 1;
    } else {
        maxXVal = maxXVal + diff*_topBottomReserve/100;
        minXVal = minXVal - diff*_topBottomReserve/100;
    }
    return @[[NSNumber numberWithFloat: minXVal],
             [NSNumber numberWithFloat: maxXVal],
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
    [_bottomGraph setNeedsLayout];
    [_mainGraph setNeedsLayout];
}

@end
