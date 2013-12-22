//
//  NCITopGraphView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCITopGraphView.h"
#import "NCISimpleGridView.m"
#import "NCITopChartView.h"
#import "NCIBtmChartView.h"
#import "NCIChartView.h"

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
    self.grid = [[NCISimpleGridView alloc] initWithGraph:self];
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
    
    self.grid.frame = CGRectMake(timeOffest * stepX, 0, self.gridWidth, self.gridHeigth);
}

@end
