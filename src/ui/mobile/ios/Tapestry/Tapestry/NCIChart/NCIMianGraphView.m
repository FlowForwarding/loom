//
//  NCIMianGraphView.m
//  Tapestry
//
//  Created by Ira on 11/20/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
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
        // Initialization code
    }
    return self;
}


-(void)scrollViewDidScroll:(UIScrollView *)scrollView{
    timePeriod = [self.chart getMaxArgument] - [self.chart getMinArgument];
    self.scaleIndex = timePeriod/
    ([self.chart.maxRangeDate timeIntervalSince1970] - [self.chart.minRangeDate timeIntervalSince1970]);
    
    
    float offsetForRanges = scrollView.contentOffset.x;
    if (scrollView.contentOffset.x < 0)
        offsetForRanges = 0;
    if (scrollView.contentOffset.x > (scrollView.contentSize.width - scrollView.frame.size.width))
        offsetForRanges = scrollView.contentSize.width - scrollView.frame.size.width;
    
    
    self.chart.minRangeDate =  [NSDate dateWithTimeIntervalSince1970:[self.chart getMinArgument] +
                                timePeriod*(offsetForRanges/scrollView.frame.size.width/self.scaleIndex)];
    
    self.chart.maxRangeDate = [NSDate dateWithTimeIntervalSince1970:[self.chart getMinArgument] +
                               timePeriod*((offsetForRanges + scrollView.frame.size.width)/scrollView.frame.size.width/self.scaleIndex)];

     // TODO redraw only if ranges chages
     [self.chart.bottomGraph redrawRanges];
    //[self.chart.mainGraph setNeedsDisplay];

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
