//
//  NCIAcitvitiesSizesView.m
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIAcitvitiesSizesView.h"
#import "NCIBarGraphView.h"

@interface NCIAcitvitiesSizesView(){
}

@property(nonatomic, strong)NCISimpleChartView *barChart;
@end

@implementation NCIAcitvitiesSizesView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        _barChart =  [[NCISimpleChartView alloc] initWithFrame:
                                         CGRectMake(100, 100, self.frame.size.width - 200, self.frame.size.height -200)
                                                                       andOptions:@{nciGraphRenderer: [NCIBarGraphView class]}
                                         ];
        
        [self addSubview:_barChart];
//        
//        for (int ind = 1; ind < 10; ind ++){
//            [_barChart addPoint:ind val:@[@(-arc4random() % 5)]];
//        }
    }
    return self;
}

- (void)loadData:(NSArray *) communities{
    [_barChart.chartData removeAllObjects];
    for (int i=0; i< communities.count; i++){
        NSDictionary* community = communities[i];
        [_barChart addPoint:i val:@[@(((NSArray *)community[@"Endpoints"]).count)]];
    }
    [_barChart drawChart];
}


@end
