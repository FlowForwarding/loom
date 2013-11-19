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

@interface NCIChartView(){
    
    NCIGraphView *mainGraph;
    NCIBottomGraphView *bottomGraph;
    bool hasSlider;
}

@end

@implementation NCIChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        hasSlider = YES;
        self.chartData = [[NSMutableArray alloc] init];
        mainGraph = [[NCIGraphView alloc] initWithChart:self];
        mainGraph.backgroundColor = [UIColor whiteColor];
        [self addSubview:mainGraph];
        
        bottomGraph = [[NCIBottomGraphView alloc] initWithChart:self];
        bottomGraph.backgroundColor = [UIColor whiteColor];
        [self addSubview:bottomGraph];

    }
    return self;
}

- (void)resetChart{
    [self.chartData removeAllObjects];
}

- (void)layoutSubviews{
    float bottomGraphHeight = 130;
    if (hasSlider){
        mainGraph.frame = CGRectMake(0, 0, self.bounds.size.width, self.bounds.size.height - bottomGraphHeight);
        bottomGraph.frame = CGRectMake(0, self.bounds.size.height - bottomGraphHeight, self.bounds.size.width, bottomGraphHeight);
    } else {
        mainGraph.frame = self.bounds;
    }
}


- (void)addPoint:(NSDate *)date val:(NSString *)value{
    NSLog(@"%@", date);
    NSLog(@"%li", (long)[value integerValue]);
    int dateSeconds = [date timeIntervalSince1970];
    if (!_minXVal || _minXVal > dateSeconds){
        _minXVal = dateSeconds;
    };
    if (!_maxXVal || _maxXVal < dateSeconds){
        _maxXVal = dateSeconds;
    };
    
    int indexValue = [value integerValue];
    if (!_minYVal || _minYVal > indexValue){
        _minYVal = indexValue;
    };
    if (!_maxYVal || _maxYVal < indexValue){
        _maxYVal = indexValue;
    };
    
    [self.chartData addObject:@[date, value]];
}

- (void)drawChart{
    [mainGraph setNeedsDisplay];
    [bottomGraph setNeedsDisplay];
}

@end
