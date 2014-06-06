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
@property(nonatomic, strong)NCITabButton *flowsByActivitiesButton;
@property(nonatomic, strong)NCITabButton *flowsPrettyButton;
@property(nonatomic, strong)NCITabButton *internalFlowsButton;
@property(nonatomic, strong)NCITabButton *activitiesButton;
@property(nonatomic, strong)NCITabButton *activitiesPrettyButton;
@property(nonatomic, strong)NCITabButton *internalActivitiesButton;
@property(nonatomic, strong)NCITabButton *activitiesSizesButton;
@property(nonatomic, strong)NCIFlowsView *flowsView;
@property(nonatomic, strong)NCIAcitvitiesSizesView *activitySizesView;
@property(nonatomic, strong)UIScrollView *buttonPanel;

@end

@implementation NCIDetailsView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        float buttonWidth = 200;
        float buttonHeight = 50;

        __weak typeof(self) weakSelf = self;
        _buttonPanel = [[UIScrollView alloc] initWithFrame:CGRectMake(0, 0, self.frame.size.width, buttonHeight)];
        _buttonPanel.contentSize = CGSizeMake(buttonWidth*8, buttonHeight);
        _buttonPanel.showsHorizontalScrollIndicator = NO;
        _buttonPanel.bounces = NO;
        [self.content addSubview:_buttonPanel];
        
        
        _flowsButton = [[NCITabButton alloc] initWithFrame:CGRectMake(0, 0, buttonWidth, buttonHeight)];
        [_flowsButton setTitle:@"Flows" forState:UIControlStateNormal];
        _flowsButton.selectAction = ^{
            [weakSelf selectTab:weakSelf.flowsButton];
            [weakSelf.flowsView showFlows];
            weakSelf.activitySizesView.hidden = YES;
            weakSelf.flowsView.hidden = NO;
        };
        _flowsButton.selected = YES;
        [_buttonPanel addSubview:_flowsButton];
        
        _flowsByActivitiesButton = [[NCITabButton alloc] initWithFrame:CGRectMake(buttonWidth, 0, buttonWidth, buttonHeight)];
        [_flowsByActivitiesButton setTitle:@"Flows(by Activities)" forState:UIControlStateNormal];
        _flowsByActivitiesButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.flowsByActivitiesButton];
            weakSelf.activitySizesView.hidden = YES;
            weakSelf.flowsView.hidden = NO;
            [weakSelf.flowsView colorifyEndpoints];
        };
        [_buttonPanel addSubview:_flowsByActivitiesButton];
        
        _flowsPrettyButton = [[NCITabButton alloc] initWithFrame:CGRectMake(2*buttonWidth, 0, buttonWidth, buttonHeight)];
        [_flowsPrettyButton setTitle:@"Flows(Pretty)" forState:UIControlStateNormal];
        _flowsPrettyButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.flowsPrettyButton];
            weakSelf.activitySizesView.hidden = YES;
            [weakSelf.flowsView applyForces];
            weakSelf.flowsView.hidden = NO;
        };
        [_buttonPanel addSubview:_flowsPrettyButton];
        
        _internalFlowsButton = [[NCITabButton alloc] initWithFrame:CGRectMake(3*buttonWidth, 0, buttonWidth, buttonHeight)];
        [_internalFlowsButton setTitle:@"Internal Flows" forState:UIControlStateNormal];
        _internalFlowsButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.internalFlowsButton];
            weakSelf.activitySizesView.hidden = YES;
            [weakSelf.flowsView  showInternal];
            weakSelf.flowsView.hidden = NO;;
        };
        [_buttonPanel addSubview:_internalFlowsButton];
        
        _activitiesButton = [[NCITabButton alloc] initWithFrame:CGRectMake(4*buttonWidth, 0, buttonWidth, buttonHeight)];
        _activitiesButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.activitiesButton];
            [weakSelf.buttonPanel setContentOffset:
             CGPointMake(weakSelf.buttonPanel.contentSize.width - weakSelf.frame.size.width, 0) animated:YES];
            weakSelf.activitySizesView.hidden = YES;
            [weakSelf.flowsView  showActivities];
            weakSelf.flowsView.hidden = NO;;
        };
        [_activitiesButton setTitle:@"Activities" forState:UIControlStateNormal];
        [_buttonPanel addSubview:_activitiesButton];
        
        _activitiesPrettyButton = [[NCITabButton alloc] initWithFrame:CGRectMake(5*buttonWidth, 0, buttonWidth, buttonHeight)];
        _activitiesPrettyButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.activitiesPrettyButton];
            weakSelf.activitySizesView.hidden = YES;
            [weakSelf.flowsView  showPrettyActivities];
            weakSelf.flowsView.hidden = NO;;
        };
        [_activitiesPrettyButton setTitle:@"Activities(Pretty)" forState:UIControlStateNormal];
        [_buttonPanel addSubview:_activitiesPrettyButton];
        
       // internalActivitiesButton
        
        _internalActivitiesButton = [[NCITabButton alloc] initWithFrame:CGRectMake(6*buttonWidth, 0, buttonWidth, buttonHeight)];
        _internalActivitiesButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.internalActivitiesButton];
            weakSelf.activitySizesView.hidden = YES;
            [weakSelf.flowsView  showInternalActivities];
            weakSelf.flowsView.hidden = NO;;
        };
        [_internalActivitiesButton setTitle:@"Internal Activities" forState:UIControlStateNormal];
        [_buttonPanel addSubview:_internalActivitiesButton];
        
        _activitiesSizesButton = [[NCITabButton alloc] initWithFrame:CGRectMake(7*buttonWidth, 0, buttonWidth, buttonHeight)];
        _activitiesSizesButton.selectAction = ^(){
            [weakSelf selectTab:weakSelf.activitiesSizesButton];
            weakSelf.activitySizesView.hidden = NO;
            weakSelf.flowsView.hidden = YES;
        };
        [_activitiesSizesButton setTitle:@"Activities Sizes" forState:UIControlStateNormal];
        [_buttonPanel addSubview:_activitiesSizesButton];
        
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

- (void)selectTab:(NCITabButton *)button{
    self.flowsButton.selected = NO;
    self.flowsByActivitiesButton.selected = NO;
    self.flowsPrettyButton.selected = NO;
    self.internalFlowsButton.selected = NO;
    self.activitiesSizesButton.selected = NO;
    self.activitiesButton.selected = NO;
    self.activitiesPrettyButton.selected = NO;
    self.internalActivitiesButton.selected = NO;
    button.selected = YES;
}

- (void)loadData:(NSDictionary *)data{
    communities = data[@"Communities"];
    self.flowsView.communityGraphData = @[data[@"CommunityGraph"]];
    [self.flowsView loadData:communities];
    [self.flowsView showFlows];
    [self.activitySizesView loadData:communities];
    generalInfo.text = [NSString stringWithFormat:@"Network Complexity Index at %@ is %@",
                    [NCIConstants processTime:data[@"Time"]], data[@"NCI"]];
}

- (void)hideActions{
    generalInfo.text = @"";
    self.flowsButton.selectAction();
    self.flowsView.updating = NO;
    self.flowsView.communitiesData = @[];
    [self.flowsView setNeedsDisplay];
}


@end
