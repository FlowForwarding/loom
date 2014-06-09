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
@property(nonatomic, strong)UIView *histogram;
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
        
        _histogram = [[UIView alloc] initWithFrame:
                     CGRectMake(0, 0, self.frame.size.width, self.frame.size.height)];
        
        _barChart =  [[NCISimpleChartView alloc] initWithFrame:
                      CGRectMake(100, 50, self.frame.size.width - 200, _activityGraph.frame.size.height -250)
                                                    andOptions:@{nciGraphRenderer: [NCIBarGraphView class],
                                                                 nciLineColors: @[[UIColor blackColor]],
                                                                 nciTapGridAction: ^(double argument, double value, float xInGrid, float yInGrid){
            int val = (argument - (int)argument) >= 0.5 ? ((int)argument + 1) : (int)argument;
            [weakSelf.activityGraph showCommunity:weakSelf.communitiesData[val]];
            [weakSelf rollHistogram];
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
        [_histogram addSubview:_barChart];
        [self addSubview:_histogram];
        
        UILabel *yAxisLabel = [[UILabel alloc] initWithFrame:CGRectMake(-250, 50, self.frame.size.height, 40)];
        yAxisLabel.text = @"Number of Endpoints per Activity X[j]";
        yAxisLabel.transform = CGAffineTransformRotate(CGAffineTransformIdentity, -M_PI/2);
        [_histogram addSubview:yAxisLabel];
        
        UILabel *xAxisLabel = [[UILabel alloc] initWithFrame:CGRectMake(0, 440, self.frame.size.width, 40)];
        xAxisLabel.textAlignment = NSTextAlignmentCenter;
        xAxisLabel.text = @"Activities Sorted by Size j";
        [_histogram addSubview:xAxisLabel];
        
        UIButton *expand = [[UIButton alloc] initWithFrame:CGRectMake(910, 400, 80, 80)];
        [expand setImage:[UIImage imageNamed:@"expand"] forState:UIControlStateNormal];
        [expand addTarget:self action:@selector(toggleHistogram) forControlEvents:UIControlEventTouchUpInside];
        [_histogram addSubview:expand];
        
        UILabel *formula = [[UILabel alloc] initWithFrame:
                            CGRectMake(0, self.frame.size.height - 100, self.frame.size.width, 50)];
        NSMutableAttributedString *formulaText = [[NSMutableAttributedString alloc] initWithString:@"NCI(N) = Max j, X[j] ≥ j\nwhere NCI(N) is the Network Complexity Index of network N and X[j] is the number of endpoints engaged in an activity"];
        [formulaText addAttribute:NSFontAttributeName value:[UIFont boldSystemFontOfSize:18] range:NSMakeRange(0,24)];
        
        formula.attributedText = formulaText;
        formula.textAlignment = NSTextAlignmentCenter;
        formula.numberOfLines = 0;
        [self addSubview:formula];
        
//
//        for (int ind = 1; ind < 10; ind ++){
//            [_barChart addPoint:ind val:@[@(-arc4random() % 5)]];
//        }
    }
    return self;
}

- (void)toggleHistogram{
    if (CGAffineTransformEqualToTransform(self.histogram.transform, CGAffineTransformIdentity)){
        [self rollHistogram];
    } else {
        [self expandHistogram];
    }
}

- (void)expandHistogram{
    __weak typeof(self) weakSelf = self;
    [UIView animateWithDuration:0.3 animations:^{
        weakSelf.histogram.transform = CGAffineTransformIdentity;
        weakSelf.histogram.center = CGPointMake((weakSelf.frame.size.width)/2, (weakSelf.frame.size.height)/2);
        [weakSelf.activityGraph showCommunity:@{}];
    } completion:^(BOOL finished) {
        
    }];
}

- (void)rollHistogram{
    __weak typeof(self) weakSelf = self;
    [UIView animateWithDuration:0.3 animations:^{
        weakSelf.histogram.transform = CGAffineTransformScale(CGAffineTransformIdentity, 0.3, 0.3);
        weakSelf.histogram.center = CGPointMake(weakSelf.frame.size.width*0.15, weakSelf.frame.size.height*0.15);
    } completion:^(BOOL finished) {
        
    }];
}

- (void)loadData:(NSArray *) communities{
    for (UIView *view in self.activityGraph.subviews){
        [view removeFromSuperview];
    }
    
    [self expandHistogram];
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
