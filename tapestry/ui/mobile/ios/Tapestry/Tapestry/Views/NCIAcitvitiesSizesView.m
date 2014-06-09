//
//  NCIAcitvitiesSizesView.m
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIAcitvitiesSizesView.h"
#import "NCIBarGraphView.h"
#import "NCIHistagramDetailsView.h"

@interface NCIAcitvitiesSizesView(){}

@property(nonatomic, strong)NSArray *communitiesData;
@property(nonatomic, strong)NCISimpleChartView *barChart;
@property(nonatomic, strong)NCIHistagramDetailsView *activityGraph;
@end

@implementation NCIAcitvitiesSizesView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    __weak typeof(self) weakSelf = self;
    if (self) {
        _activityGraph = [[NCIHistagramDetailsView alloc] initWithFrame:
                          CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        [self addSubview:_activityGraph];
        _activityGraph.backgroundColor = [UIColor clearColor];
        
        _barChart =  [[NCISimpleChartView alloc] initWithFrame:
                      CGRectMake(100, 100, self.frame.size.width - 200, self.frame.size.height -200)
                                                    andOptions:@{nciGraphRenderer: [NCIBarGraphView class],
                                                                 nciLineColors: @[[UIColor blackColor]],
                                                                 nciTapGridAction: ^(double argument, double value, float xInGrid, float yInGrid){
            int val = (argument - (int)argument) >= 0.5 ? ((int)argument + 1) : (int)argument;
            [weakSelf.activityGraph showCommunity:weakSelf.communitiesData[val]];
            [UIView animateWithDuration:0.5 animations:^{
                weakSelf.barChart.transform = CGAffineTransformScale(CGAffineTransformIdentity, 0.3, 0.3);
                weakSelf.barChart.center = CGPointMake((weakSelf.frame.size.width - 200)*0.15,
                                                       (weakSelf.frame.size.height - 200)*0.15);
            } completion:^(BOOL finished) {
                
            }];
        },
                                                                 nciXAxis: @{
                                                                         nciLabelRenderer:^(double value){
            
            return [[NSAttributedString alloc] initWithString: [NSString stringWithFormat:@"%d", (int)value]];
        }
                                                                         },
                                                                 nciYAxis: @{
                                                                         nciLabelRenderer:^(double value){
            
            return [[NSAttributedString alloc] initWithString: [NSString stringWithFormat:@"%d", (int)value]];
        }}
                                                                 }
                      ];
        _barChart.backgroundColor = [UIColor colorWithWhite:0.1 alpha:0.1];
        [self addSubview:_barChart];
        
//
//        for (int ind = 1; ind < 10; ind ++){
//            [_barChart addPoint:ind val:@[@(-arc4random() % 5)]];
//        }
    }
    return self;
}

- (void)loadData:(NSArray *) communities{
    for (UIView *view in self.activityGraph.subviews){
        [view removeFromSuperview];
    }

    self.barChart.transform = CGAffineTransformIdentity;
    self.barChart.center = CGPointMake((self.frame.size.width - 200)/2 + 100,
                                  (self.frame.size.height - 200)/2 + 100);
    _communitiesData = communities;
    [_barChart.chartData removeAllObjects];
    for (int i=0; i< communities.count; i++){
        NSDictionary* community = communities[i];
        [_barChart addPoint:i val:@[@(((NSArray *)community[@"Endpoints"]).count)]];
    }
    [_barChart drawChart];
    [_activityGraph  showCommunity:@{}];
}


@end
