//
//  NCISimpleGraphView.m
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCISimpleGraphView.h"
#import "NCISimpleChartView.h"
#import "NCISimpleGridView.m"

@interface NCISimpleGraphView(){
    float gridHeigth;
    float gridWidth;
    
}

@end

@implementation NCISimpleGraphView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        _xLabelsDistance = 200;
        _yLabelsDistance = 50;
        _xLabelsWidth = 100;
        _yLabelsHeigth = 20;
        _yAxisLabels = [[NSMutableArray alloc] init];
        _xAxisLabels = [[NSMutableArray alloc] init];
        self.backgroundColor = [UIColor clearColor];
        
        _grid = [[NCISimpleGridView alloc] initWithGraph:self];
        [self addSubview:_grid];
    }
    return self;
}

- (id)initWithChart: (NCISimpleChartView *)chartHolder{
    self = [self initWithFrame:CGRectZero];
    if (self){
        _chart = chartHolder;
    }
    return  self;
}

- (void)layoutSubviews{
    for (UILabel *label in _yAxisLabels){
        [label removeFromSuperview];
    }
    for (UILabel *label in _xAxisLabels){
        [label removeFromSuperview];
    }
    [_yAxisLabels removeAllObjects];
    [_xAxisLabels removeAllObjects];
    
    gridHeigth = self.frame.size.height- _yLabelsHeigth;
    gridWidth = self.frame.size.width - _xLabelsWidth;
    if (_chart.chartData.count > 0){
        NSArray *yVals = [_chart getBoundaryValues];
        _minYVal = [yVals[0] floatValue];
        _maxYVal = [yVals[1] floatValue];
        _yStep = gridHeigth/(_maxYVal - _minYVal);
        _minXVal = [_chart.chartData[0][0] timeIntervalSince1970];
        _maxXVal = [[_chart.chartData lastObject][0] timeIntervalSince1970];
        _xStep = gridWidth/(_maxXVal - _minXVal);
        
        for(int i = 0; i<= gridHeigth/_yLabelsDistance; i++){
            UILabel *label = [[UILabel alloc] initWithFrame:CGRectMake(0, self.frame.size.height - i*_yLabelsDistance - _yLabelsHeigth, _xLabelsWidth, 20)];
            label.text = [@([self getDateByY: (_yLabelsHeigth + _yLabelsDistance *i)]) description];
            [_yAxisLabels addObject:label];
            [self addSubview:label];
        }
        
        for(int i = 0; i<= gridWidth/_xLabelsDistance; i++){
            UILabel *label = [[UILabel alloc] initWithFrame:
                              CGRectMake(_xLabelsWidth + _xLabelsDistance *i,
                                         self.frame.size.height - _yLabelsHeigth, _xLabelsDistance,
                                         _yLabelsHeigth)];
            label.text = [[self getDateByX: (_xLabelsWidth + _xLabelsDistance *i)] description];
            [_xAxisLabels addObject:label];
            [self addSubview:label];
        }
    }
    _grid.frame = CGRectMake(_xLabelsWidth, 0, gridWidth, gridHeigth);
   // [grid setNeedsDisplay];
}


- (NSDate *)getDateByX:(float) pointX{
    return [NSDate dateWithTimeIntervalSince1970:(_minXVal + (pointX - _xLabelsWidth)/_xStep)];
}

- (float )getDateByY:(float) pointY{
    return _minYVal + (pointY - _yLabelsHeigth)/_yStep;
}



@end
