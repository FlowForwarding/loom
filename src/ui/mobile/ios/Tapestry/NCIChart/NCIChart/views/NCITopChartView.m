//
//  NCITopChartView.m
//  NCIChart
//
//  Created by Ira on 12/22/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCITopChartView.h"
#import "NCITopGraphView.h"

@interface NCITopChartView(){
}

@end

@implementation NCITopChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}

- (void)addSubviews{
    self.graph = [[NCITopGraphView alloc] initWithChart:self];
    [self addSubview:self.graph];
}

/*
// Only override drawRect: if you perform custom drawing.
// An empty implementation adversely affects performance during animation.
- (void)drawRect:(CGRect)rect
{
    // Drawing code
}
*/

@end
