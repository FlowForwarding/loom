//
//  NCIMianGraphView.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIMianGraphView.h"
#import "NCIBottomGraphView.h"

@interface NCIMianGraphView(){
    float timePeriod;
}

@property(nonatomic)double implicitWidth;

@end

@implementation NCIMianGraphView

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
            [self.chart.bottomGraph startMoveWithPoint:point1 andPoint:point2];
        }
    }
    if ([(UIPinchGestureRecognizer *)sender state] == UIGestureRecognizerStateChanged) {
        if ([sender numberOfTouches] == 2) {
            CGPoint point1 = [(UIPinchGestureRecognizer *)sender locationOfTouch:0 inView:self];
            CGPoint point2 = [(UIPinchGestureRecognizer *)sender locationOfTouch:1 inView:self];
            [self.chart.bottomGraph moveReverseRangesWithPoint:point1 andPoint:point2];
        }
    }

}


-(void)scrollViewDidScroll:(UIScrollView *)scrollView{
    if (self.chart.chartData.count < 2)
        return;
    timePeriod = [self.chart getMaxArgument] - [self.chart getMinArgument];
    double rangesDiff = [self.chart.maxRangeDate timeIntervalSince1970] - [self.chart.minRangeDate timeIntervalSince1970];
    self.scaleIndex = timePeriod/rangesDiff;
    
    float offsetForRanges = scrollView.contentOffset.x;
    if (offsetForRanges < 0)
        offsetForRanges = 0;
    if (offsetForRanges > (scrollView.contentSize.width - scrollView.frame.size.width))
        offsetForRanges = scrollView.contentSize.width - scrollView.frame.size.width;

    double newMinRange = [self.chart getMinArgument] + timePeriod*(offsetForRanges/scrollView.frame.size.width/self.scaleIndex);
    self.chart.minRangeDate =  [NSDate dateWithTimeIntervalSince1970:newMinRange];
    self.chart.maxRangeDate = [NSDate dateWithTimeIntervalSince1970:newMinRange + rangesDiff];
    
    [self.chart.bottomGraph redrawRanges];

}


- (void)layoutSubviews{
    
    [super layoutSubviews];
    timePeriod = [self.chart getMaxArgument] - [self.chart getMinArgument];
    self.scaleIndex = timePeriod/
        ([self.chart.maxRangeDate timeIntervalSince1970] - [self.chart.minRangeDate timeIntervalSince1970]);

    if (self.scaleIndex > 0 ){
        
        self.gridScroll.frame = CGRectMake(self.leftRightIndent,self.topChartIndent, self.frame.size.width - 2*self.leftRightIndent,
                                           self.frame.size.height - self.topChartIndent);
        
        _implicitWidth = (self.frame.size.width - 2*self.leftRightIndent)*self.scaleIndex;
        
        
        self.gridScroll.contentSize =
          CGSizeMake(_implicitWidth, self.frame.size.height - self.topChartIndent - self.bottomChartIndent);
        
        float gridStep = timePeriod/self.gridScroll.contentSize.width;
        self.leftShift = ([self.chart getMaxArgument] - [self.chart.maxRangeDate timeIntervalSince1970])/gridStep;
        
        self.gridArea.frame = CGRectMake((self.scaleIndex -1)*(self.frame.size.width - 2*self.leftRightIndent) - self.leftShift,
                                         0, self.frame.size.width,
                                         self.frame.size.height - self.topChartIndent - self.bottomChartIndent);
        
        [self.gridScroll setContentOffset:CGPointMake((self.scaleIndex -1)*(self.frame.size.width - 2*self.leftRightIndent) - self.leftShift, 0)];

        //[self.chart.bottomGraph redrawRanges];
    }
    
}



@end
