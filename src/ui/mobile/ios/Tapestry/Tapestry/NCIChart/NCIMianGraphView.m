//
//  NCIMianGraphView.m
//  Tapestry
//
//  Created by Ira on 11/20/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIMianGraphView.h"
#import "NCIBottomGraphView.h"

@implementation NCIMianGraphView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}


-(void)scrollViewDidScroll:(UIScrollView *)scrollView{
    float timePeriod = [self.chart getMaxArgument] - [self.chart getMinArgument];
    
    self.scaleIndex = timePeriod/
    ([self.chart.maxRangeDate timeIntervalSince1970] - [self.chart.minRangeDate timeIntervalSince1970]);
    
    self.chart.minRangeDate =  [NSDate dateWithTimeIntervalSince1970:[self.chart getMinArgument] +
                                timePeriod*(scrollView.contentOffset.x/scrollView.frame.size.width/self.scaleIndex)];
    
    self.chart.maxRangeDate = [NSDate dateWithTimeIntervalSince1970:[self.chart getMinArgument] +
                               timePeriod*((scrollView.contentOffset.x + scrollView.frame.size.width)/scrollView.frame.size.width/self.scaleIndex)];

    [self.chart.bottomGraph redrawRanges];

}

- (void)layoutSubviews{
    
    [super layoutSubviews];
    self.scaleIndex = ([self.chart getMaxArgument] - [self.chart getMinArgument])/
        ([self.chart.maxRangeDate timeIntervalSince1970] - [self.chart.minRangeDate timeIntervalSince1970]);

    if (self.scaleIndex > 0 ){
        self.gridScroll.frame = CGRectMake(self.leftRightIndent,self.topChartIndent, self.frame.size.width - 2*self.leftRightIndent,
                                           self.frame.size.height - self.topChartIndent);
        self.gridScroll.contentSize =
          CGSizeMake((self.frame.size.width - 2*self.leftRightIndent)*self.scaleIndex, self.frame.size.height - self.topChartIndent - self.bottomChartIndent);
        self.gridArea.frame = CGRectMake(0, 0, (self.frame.size.width - 2*self.leftRightIndent)*self.scaleIndex,
                                         self.frame.size.height - self.topChartIndent - self.bottomChartIndent);
        
        float gridStep = ([self.chart getMaxArgument] - [self.chart getMinArgument])/self.gridScroll.contentSize.width;
        self.leftShift = ([self.chart getMaxArgument] - [self.chart.maxRangeDate timeIntervalSince1970])/gridStep;

        [self.gridScroll setContentOffset:CGPointMake((self.scaleIndex -1)*(self.frame.size.width - 2*self.leftRightIndent) - self.leftShift, 0)];
    }
    
}

@end
