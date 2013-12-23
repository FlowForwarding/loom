//
//  NCITopGraphView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCITopGraphView.h"
#import "NCITopGridView.h"
#import "NCIBtmChartView.h"
#import "NCIChartView.h"
#import "NCITopChartView.h"

@interface NCITopGraphView()<UIScrollViewDelegate>{
    UIScrollView *gridScroll;
}
@end

@implementation NCITopGraphView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        
    }
    return self;
}

- (void)addSubviews{
    gridScroll = [[UIScrollView alloc] initWithFrame:CGRectZero];
    [self addSubview:gridScroll];
    gridScroll.delegate = self;
    self.grid = [[NCITopGridView alloc] initWithGraph:self];
    [gridScroll addSubview:self.grid];
}

-(void)scrollViewDidScroll:(UIScrollView *)scrollView{
    NCIChartView *nciChart = ((NCITopChartView *)self.chart).nciChart;
    float scaleIndex = [nciChart getScaleIndex];
    float timePeriod = [nciChart getTimePeriod];
    
    //TODO mabe this courses gap
    float rangesPeriod = [nciChart getRangesPeriod];
    
    float offsetForRanges = scrollView.contentOffset.x;
    if (offsetForRanges < 0)
        offsetForRanges = 0;
    if (offsetForRanges > (scrollView.contentSize.width - scrollView.frame.size.width))
        offsetForRanges = scrollView.contentSize.width - scrollView.frame.size.width;
    
    double newMinRange = [self.chart.chartData[0][0] timeIntervalSince1970] +
        timePeriod*(offsetForRanges/scrollView.frame.size.width/scaleIndex);
    nciChart.minRangeDate = [NSDate dateWithTimeIntervalSince1970:newMinRange];
    nciChart.maxRangeDate = [NSDate dateWithTimeIntervalSince1970:newMinRange + rangesPeriod];
    
     self.grid.frame = CGRectMake(gridScroll.contentOffset.x, 0, self.gridWidth, self.gridHeigth);

    [self setNeedsLayout];
    [self.grid setNeedsDisplay];
    [self.chart layoutSelectedPoint];
    [nciChart.btmChart redrawRanges];
    
}

- (void)layoutSubviews{
    [super layoutSubviews];
    float scaleIndex = [((NCITopChartView *)self.chart).nciChart getScaleIndex];
    float contentWidth = self.gridWidth* scaleIndex;
    
    float stepX = contentWidth/
    ([[self.chart.chartData lastObject][0] timeIntervalSince1970] - [self.chart.chartData[0][0] timeIntervalSince1970]);
    
    gridScroll.frame = CGRectMake(self.xLabelsWidth, 0, self.gridWidth, self.gridHeigth);
    
    gridScroll.contentSize = CGSizeMake(contentWidth, self.gridHeigth);
    
    float timeOffest = [((NCITopChartView *)self.chart).nciChart.minRangeDate timeIntervalSince1970] -
        [self.chart.chartData[0][0] timeIntervalSince1970];
    if (timeOffest < 0)
        timeOffest = 0;
    gridScroll.contentOffset = CGPointMake(timeOffest * stepX, 0);
     [self.chart layoutSelectedPoint];
    self.grid.frame = CGRectMake(timeOffest * stepX, 0, self.gridWidth, self.gridHeigth);
}

- (void)redrawXLabels{
    float scaleIndex = [((NCITopChartView *)self.chart).nciChart getScaleIndex];
    for(int i = 0; i<= self.gridWidth/self.xLabelsDistance; i++){
        UILabel *label = [[UILabel alloc] initWithFrame:
                          CGRectMake(self.xLabelsWidth + self.xLabelsDistance *i,
                                     self.frame.size.height - self.yLabelsHeigth, self.xLabelsDistance,
                                     self.yLabelsHeigth)];
        label.text = [[self getDateByX: (gridScroll.contentOffset.x + self.xLabelsWidth + self.xLabelsDistance *i)/scaleIndex] description];
        [self.xAxisLabels addObject:label];
        [self addSubview:label];
    }
}

- (NSDate *)getDateByX:(float) pointX{
    float scaleIndex = [((NCITopChartView *)self.chart).nciChart getScaleIndex];
    return [NSDate dateWithTimeIntervalSince1970:(self.minXVal + (gridScroll.contentOffset.x + pointX - self.xLabelsWidth)/scaleIndex/self.xStep)];
}

- (float)getXValueByDate:(NSDate *)date{
    float scaleIndex = [((NCITopChartView *)self.chart).nciChart getScaleIndex];
    return ([date timeIntervalSince1970] - self.minXVal)*self.xStep * scaleIndex - gridScroll.contentOffset.x;
}

- (void)detectRanges{

    NSArray *yVals = [((NCITopChartView *)self.chart) getValsInRanges];
    self.minYVal = [yVals[0] floatValue];
    self.maxYVal = [yVals[1] floatValue];
    self.yStep = self.gridHeigth/(self.maxYVal - self.minYVal);
}

@end
