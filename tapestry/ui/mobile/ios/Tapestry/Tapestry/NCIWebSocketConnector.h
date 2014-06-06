//
//  NCIWebSocketConnector.h
//  Tapestry
//
//  Created by Infoblox Inc on 11/21/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import <Foundation/Foundation.h>
#import "NCIIndexValueView.h"
#import "NCIEditServerView.h"
#import "NCIChartView.h"
#import "NCICollectorsDetailsView.h"
#import "NCIDetailsView.h"
#import "NCIPeriodSwitcherPanel.h"

@interface NCIWebSocketConnector : NSObject

+ (NCIWebSocketConnector *)interlocutor;

- (void)reconnect;
- (void)resetData;
- (void)requestLastDataForPeiodInSeconds:(float) period;
- (void)requestCollecotrsDetails:(NSString *) date;
- (void)requestNCIDetails:(NSString *) date;

- (void)newTapestryUrl:(NSString *) newUrl;
- (NSString *)getTapestryUrl;
- (void)removeURLAtIndex:(long)index;


@property(nonatomic, strong) NCIEditServerView *editServerView;
@property(nonatomic, strong) NCIIndexValueView *nciValue;
@property(nonatomic, strong) NCIIndexValueView *nepValue;
@property(nonatomic, strong) NCIIndexValueView *qpsValue;
@property(nonatomic, strong) NCIIndexValueView *collectorsValue;
@property(nonatomic, strong) NCIChartView *chartView;
@property(nonatomic, strong) NCIPeriodSwitcherPanel *periodSwitcherPanel;
@property(nonatomic, strong) NCICollectorsDetailsView *collectorsDetailsView;
@property(nonatomic, strong) NCIDetailsView *detailsView;
@property(nonatomic, strong) UIButton *noConnection;
@property(nonatomic, strong) UILabel *progressLabel;

@property(nonatomic, strong) NSDate *startDate;
@property(nonatomic) float currentDatePeriod;

@property(nonatomic, strong) NSMutableArray *tapestryURLs;



@end
