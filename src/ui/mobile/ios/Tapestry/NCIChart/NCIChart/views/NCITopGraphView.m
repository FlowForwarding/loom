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
        UIPinchGestureRecognizer *croperViewGessture = [[UIPinchGestureRecognizer alloc]initWithTarget:self action:@selector(croperViewScale:)];
        [self addGestureRecognizer:croperViewGessture];
        // Initialization code
    }
    return self;
}

-(void)croperViewScale:(id)sender
{
    if (self.chart.chartData.count < 2)
        return;
    if([(UIPinchGestureRecognizer *)sender state]==UIGestureRecognizerStateBegan)
    {
        
        if ([sender numberOfTouches] == 2) {
            CGPoint point1 = [(UIPinchGestureRecognizer *)sender locationOfTouch:0 inView:self];
            CGPoint point2 = [(UIPinchGestureRecognizer *)sender locationOfTouch:1 inView:self];
            [((NCITopChartView *)self.chart).nciChart.btmChart startMoveWithPoint:point1 andPoint:point2];
        }
    }
    if ([(UIPinchGestureRecognizer *)sender state] == UIGestureRecognizerStateChanged) {
        if ([sender numberOfTouches] == 2) {
            CGPoint point1 = [(UIPinchGestureRecognizer *)sender locationOfTouch:0 inView:self];
            CGPoint point2 = [(UIPinchGestureRecognizer *)sender locationOfTouch:1 inView:self];
            [((NCITopChartView *)self.chart).nciChart.btmChart moveReverseRangesWithPoint:point1 andPoint:point2];
        }
    }
    
}

- (void)addSubviews{
    gridScroll = [[UIScrollView alloc] initWithFrame:CGRectZero];
    [gridScroll setShowsVerticalScrollIndicator:NO];
    [self addSubview:gridScroll];
    gridScroll.delegate = self;
    self.grid = [[NCITopGridView alloc] initWithGraph:self];
    [gridScroll addSubview:self.grid];
}

-(void)scrollViewDidScroll:(UIScrollView *)scrollView{
    NCIChartView *nciChart = ((NCITopChartView *)self.chart).nciChart;
    float scaleIndex = [nciChart getScaleIndex];
    float timePeriod = [nciChart getTimePeriod];
    float rangesPeriod = [nciChart getRangesPeriod];
    
    float offsetForRanges = scrollView.contentOffset.x;
    if (offsetForRanges < 0)
        offsetForRanges = 0;
    if (offsetForRanges > (scrollView.contentSize.width - scrollView.frame.size.width))
        offsetForRanges = scrollView.contentSize.width - scrollView.frame.size.width;
    
    double newMinRange = [self.chart.chartData[0][0] doubleValue] +
        timePeriod*(offsetForRanges/scrollView.frame.size.width/scaleIndex);
    nciChart.minRangeVal = newMinRange;
    nciChart.maxRangeVal = newMinRange + rangesPeriod;
    
     self.grid.frame = CGRectMake(gridScroll.contentOffset.x, 0, self.gridWidth, self.gridHeigth);

    [self setNeedsLayout];
    [self.grid setNeedsDisplay];
    [self.chart layoutSelectedPoint];
    [nciChart.btmChart redrawRanges];
    
}

- (void)layoutSubviews{

    [super layoutSubviews];
    if (self.chart.chartData.count < 2)
            return;
    float scaleIndex = [((NCITopChartView *)self.chart).nciChart getScaleIndex];
    float contentWidth = self.gridWidth* scaleIndex;
    float timeDiff = [[self.chart.chartData lastObject][0] doubleValue] - [self.chart.chartData[0][0] doubleValue];
    if (timeDiff == 0){
        timeDiff = contentWidth;
    }
    float stepX = contentWidth/timeDiff;
    
    gridScroll.frame = CGRectMake(self.xLabelsWidth, 0, self.gridWidth, self.gridHeigth);
    
    gridScroll.contentSize = CGSizeMake(contentWidth, self.gridHeigth);
    
    double timeOffest = ((NCITopChartView *)self.chart).nciChart.minRangeVal  -  [self.chart.chartData[0][0] doubleValue];
    if (timeOffest < 0)
        timeOffest = 0;
    gridScroll.contentOffset = CGPointMake(timeOffest * stepX, 0);
     [self.chart layoutSelectedPoint];
    self.grid.frame = CGRectMake(timeOffest * stepX, 0, self.gridWidth, self.gridHeigth);
}

- (void)redrawXLabels{
    float scaleIndex = [((NCITopChartView *)self.chart).nciChart getScaleIndex];
    float xLabelsDistance = self.chart.nciXLabelsDistance;
    if ((1/self.xStep/scaleIndex * xLabelsDistance) < 60*60*24){
        [self.dateFormatter setDateFormat:@"yyyy-MMM-dd HH:mm"];
    } else if ((1/self.xStep/scaleIndex * xLabelsDistance) < 60*60*24*30){
        [self.dateFormatter setDateFormat:@"yyyy-MMM-dd"];
    } else {
        [self.dateFormatter setDateFormat:@"yyyy-MMM"];
    }
    
    float shift =  gridScroll.contentOffset.x - xLabelsDistance*((int)gridScroll.contentOffset.x / (int)xLabelsDistance);
    for(int i = 0; i<= self.gridWidth/xLabelsDistance + 1; i++){
        float xVal = self.xLabelsWidth + xLabelsDistance *i - shift;
        if ((xVal - self.xLabelsWidth) >= 0 && (xVal < self.frame.size.width) ){
            UILabel *label = [[UILabel alloc] initWithFrame:
                              CGRectMake(xVal - xLabelsDistance/2,
                                         self.frame.size.height - self.yLabelsHeigth, xLabelsDistance ,
                                         self.yLabelsHeigth)];
            label.font = self.chart.nciXLabelsFont;
            label.textAlignment = NSTextAlignmentCenter;
            label.text = [NSString stringWithFormat:@"%@", [self.dateFormatter stringFromDate:
                                                    [NSDate dateWithTimeIntervalSince1970:[self getArgumentByX: xVal -  self.xLabelsWidth]]]];
            
            [self.xAxisLabels addObject:label];
            [self addSubview:label];
        }
    }
}

- (double)getArgumentByX:(float) pointX{
    float scaleIndex = [((NCITopChartView *)self.chart).nciChart getScaleIndex];
    return self.minXVal + (gridScroll.contentOffset.x + pointX)/scaleIndex/self.xStep;
}

- (float)getXByArgument:(double) arg{
    float scaleIndex = [((NCITopChartView *)self.chart).nciChart getScaleIndex];
    return (arg - self.minXVal)*self.xStep * scaleIndex - gridScroll.contentOffset.x;
}

- (void)detectRanges{
    NSArray *yVals = [((NCITopChartView *)self.chart) getValsInRanges];
    self.minYVal = [yVals[0] floatValue];
    self.maxYVal = [yVals[1] floatValue];
    self.yStep = self.gridHeigth/(self.maxYVal - self.minYVal);
}

@end
