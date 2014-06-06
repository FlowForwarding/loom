//
//  NCIDetailsView.m
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIDetailsView.h"
#import "NCITabButton.h"
#import "NCIAcitvitiesSizesView.h"
#import "NCIFlowsView.h"

@interface NCIDetailsView(){
    UILabel *generalInfo;
    NSArray *communities;
}

@property(nonatomic, strong)NCITabButton *flowsButton;
@property(nonatomic, strong)NCITabButton *activitiesButton;
@property(nonatomic, strong)NCITabButton *activitiesPrettyButton;
@property(nonatomic, strong)NCITabButton *internalNetworkButton;
@property(nonatomic, strong)NCITabButton *activitiesSizesButton;
@property(nonatomic, strong)NCIFlowsView *flowsView;
@property(nonatomic, strong)NCIAcitvitiesSizesView *activitySizesView;

@end

@implementation NCIDetailsView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        float buttonWidth = frame.size.width/5;
        float buttonHeight = 50;

        __weak typeof(self) weakSelf = self;
        _flowsButton = [[NCITabButton alloc] initWithFrame:CGRectMake(0, 0, buttonWidth, buttonHeight)];
        [_flowsButton setTitle:@"Flows" forState:UIControlStateNormal];
        _flowsButton.selectAction = ^{
            [weakSelf selectTab:weakSelf.flowsButton];
            [weakSelf showFlows];
        };
        _flowsButton.selected = YES;
        [self.content addSubview:_flowsButton];
        
        _activitiesButton = [[NCITabButton alloc] initWithFrame:CGRectMake(buttonWidth, 0, buttonWidth, buttonHeight)];
        [_activitiesButton setTitle:@"Activities" forState:UIControlStateNormal];
        _activitiesButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.activitiesButton];
            [weakSelf showActivities];
        };
        [self.content addSubview:_activitiesButton];
        
        _activitiesPrettyButton = [[NCITabButton alloc] initWithFrame:CGRectMake(2*buttonWidth, 0, buttonWidth, buttonHeight)];
        [_activitiesPrettyButton setTitle:@"Activities(Pretty)" forState:UIControlStateNormal];
        _activitiesPrettyButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.activitiesPrettyButton];
            [weakSelf showPrettyActivitites];
        };
        [self.content addSubview:_activitiesPrettyButton];
        
        _internalNetworkButton = [[NCITabButton alloc] initWithFrame:CGRectMake(3*buttonWidth, 0, buttonWidth, buttonHeight)];
        [_internalNetworkButton setTitle:@"Internal Network" forState:UIControlStateNormal];
        _internalNetworkButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.internalNetworkButton];
            [weakSelf showInternalNetwork];
        };
        [self.content addSubview:_internalNetworkButton];
        
        _activitiesSizesButton = [[NCITabButton alloc] initWithFrame:CGRectMake(4*buttonWidth, 0, buttonWidth, buttonHeight)];
        _activitiesSizesButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.activitiesSizesButton];
            [weakSelf activitiesSizes];
        };
        [_activitiesSizesButton setTitle:@"Activities Sizes" forState:UIControlStateNormal];
        [self.content addSubview:_activitiesSizesButton];
        
        generalInfo = [[UILabel alloc] initWithFrame:CGRectMake(0, 50, self.content.frame.size.width, 50)];
        generalInfo.textAlignment = NSTextAlignmentCenter;
        [self.content addSubview:generalInfo];
        
        _activitySizesView = [[NCIAcitvitiesSizesView alloc]
                              initWithFrame:CGRectMake(0, 100, self.frame.size.width, self.frame.size.height - 100)];
        _activitySizesView.hidden = YES;
        [self.content addSubview:_activitySizesView];
        _flowsView = [[NCIFlowsView alloc]
                      initWithFrame:CGRectMake(0, 100, self.frame.size.width, self.frame.size.height - 100)];
        [self.content addSubview:_flowsView];
        
    }
    return self;
}

- (void)showFlows {
    _activitySizesView.hidden = YES;
    _flowsView.hidden = NO;
}

- (void)showActivities {
    _activitySizesView.hidden = YES;
    _flowsView.hidden = NO;
}

- (void)showPrettyActivitites {
    _activitySizesView.hidden = YES;
    _flowsView.hidden = NO;
}

- (void)showInternalNetwork {
    _activitySizesView.hidden = YES;
    _flowsView.hidden = NO;
}

- (void)activitiesSizes{
    _activitySizesView.hidden = NO;
    _flowsView.hidden = YES;
}

- (void)selectTab:(NCITabButton *)button{
    self.flowsButton.selected = NO;
    self.activitiesButton.selected = NO;
    self.activitiesPrettyButton.selected = NO;
    self.internalNetworkButton.selected = NO;
    self.activitiesSizesButton.selected = NO;
    button.selected = YES;
}

- (void)loadData:(NSDictionary *)data{
    communities = data[@"Communities"];
    [self.flowsView loadData:communities];
    [self.activitySizesView loadData:communities];
    generalInfo.text = [NSString stringWithFormat:@"Network Complexity Index at %@ is %@",
                    [NCIConstants processTime:data[@"Time"]], data[@"NCI"]];
}

- (void)hideActions{
    generalInfo.text = @"";
    self.flowsButton.selectAction();
}


@end
