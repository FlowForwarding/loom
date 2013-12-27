//
//  NCISimpleGraphView.m
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCISimpleGraphView.h"
#import "NCISimpleGridView.h"

@interface NCISimpleGraphView(){
    
}

@end

@implementation NCISimpleGraphView

//TODO for simple chart
- (NSDate *)getDateByXValue:(float)xVal{
    return nil;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {

        _yLabelShift = 15;
        _xLabelsWidth = 50;
        
        if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad){
            _yLabelsHeigth = 40;
            _xLabelsWidth = 50;
        } else {
            _yLabelsHeigth = 20;
            _xLabelsWidth = 30;
        }
        
        
        _yAxisLabels = [[NSMutableArray alloc] init];
        _xAxisLabels = [[NSMutableArray alloc] init];
        self.backgroundColor = [UIColor clearColor];
        _dateFormatter = [[NSDateFormatter alloc] init];
        [self addSubviews];
    }
    return self;
}

- (void)addSubviews{
    self.grid = [[NCISimpleGridView alloc] initWithGraph:self];
    [self addSubview:self.grid];
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
    
    _gridHeigth = self.frame.size.height- _yLabelsHeigth;
    _gridWidth = self.frame.size.width - _xLabelsWidth;
    float yLabelsDistance = self.chart.nciYLabelsDistance;
    
    if (_chart.chartData.count > 0){
        _minXVal = [_chart.chartData[0][0] timeIntervalSince1970];
        _maxXVal = [[_chart.chartData lastObject][0] timeIntervalSince1970];
        _xStep = _gridWidth/(_maxXVal - _minXVal);
        [self detectRanges];
       
        for(int i = 0; i<= _gridHeigth/yLabelsDistance; i++){
            UILabel *label = [[UILabel alloc] initWithFrame:
                              CGRectMake(0, self.frame.size.height - i*yLabelsDistance - _yLabelsHeigth - _yLabelShift, _xLabelsWidth, 20)];
            label.font =  self.chart.nciYLabelsFont;
            if (self.chart.hasYLabels){
                label.text = [NSString stringWithFormat:@"%.1f", [self getValByY: (_yLabelsHeigth + yLabelsDistance*i)]];
            }
            [_yAxisLabels addObject:label];
            [self addSubview:label];
        }
       
        [self redrawXLabels];
    }
    _grid.frame = CGRectMake(_xLabelsWidth, 0, _gridWidth, _gridHeigth);
   [_grid setNeedsDisplay];
}

- (void)detectRanges{
    NSArray *yVals = [_chart getBoundaryValues];
    _minYVal = [yVals[0] floatValue];
    _maxYVal = [yVals[1] floatValue];
    _yStep = _gridHeigth/(_maxYVal - _minYVal);
}

- (void)redrawXLabels{
    float xLabelsDistance = self.chart.nciXLabelsDistance;
    
    if ((1/_xStep * xLabelsDistance) < 60*60*24){
        [_dateFormatter setDateFormat:@"yyyy-MMM-dd HH:mm"];
    } else if ((1/_xStep * xLabelsDistance) < 60*60*24*30){
        [_dateFormatter setDateFormat:@"yyyy-MMM-dd"];
    } else {
        [_dateFormatter setDateFormat:@"yyyy-MMM"];
    }
    
    for(int i = 0; i<= _gridWidth/xLabelsDistance; i++){
        UILabel *label = [[UILabel alloc] initWithFrame:
                          CGRectMake(_xLabelsWidth + xLabelsDistance *i  - xLabelsDistance/2,
                                     self.frame.size.height - _yLabelsHeigth, xLabelsDistance,
                                     _yLabelsHeigth)];
        label.textAlignment = NSTextAlignmentCenter;
        label.font = self.chart.nciXLabelsFont;
        label.text = [NSString stringWithFormat:@"%@", [_dateFormatter stringFromDate: [self getDateByX: (_xLabelsWidth + xLabelsDistance *i)]]];
        [_xAxisLabels addObject:label];
        [self addSubview:label];
    }
}


- (NSDate *)getDateByX:(float) pointX{
    return [NSDate dateWithTimeIntervalSince1970:(_minXVal + (pointX)/_xStep)];
}

- (float )getValByY:(float) pointY{
    return _minYVal + (pointY - _yLabelsHeigth)/_yStep;
}

- (CGPoint)pointByServerDataInGrid:(NSArray *)data{
    NSDate *date = data[0];
    if ([data[1] isKindOfClass:[NSNull class]] )
        return CGPointZero;
    float yVal = self.frame.size.height - (([data[1] floatValue] - _minYVal)*_yStep) - _yLabelsHeigth;
    float xVal = [self getXValueByDate: date];
    return CGPointMake(xVal, yVal);
}

- (float)getXValueByDate:(NSDate *)date{
    return ([date timeIntervalSince1970] - _minXVal)*_xStep;
}

@end
