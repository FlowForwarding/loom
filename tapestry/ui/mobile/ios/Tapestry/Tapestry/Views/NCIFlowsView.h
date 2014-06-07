//
//  NCIFlowsView.h
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface NCIFlowsView : UIView

- (void)loadData:(NSArray *) communities;
- (void)showFlows;
- (void)colorifyEndpoints;
- (void)showPrettyFlows;
- (void)showInternalFlows;

- (void)showActivities;
- (void)showPrettyActivities;
- (void)showInternalActivities;
@property(nonatomic, strong) NSArray *communitiesData;
@property(nonatomic, strong) NSArray *communityGraphData;
@property(nonatomic) BOOL updating;

@end
