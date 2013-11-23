//
//  NCIWebSocketConnector.h
//  Tapestry
//
//  Created by Ira on 11/21/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import <Foundation/Foundation.h>
#import "NCIIndexValueView.h"
#import "NCIEditServerView.h"
#import "NCIChartView.h"

@interface NCIWebSocketConnector : NSObject

+ (NCIWebSocketConnector *)interlocutor;

- (void)reconnect;
- (void)resetData;
- (void)requestLastDataForPeiodInSeconds:(int) period;

@property(nonatomic, strong) NCIEditServerView *editServerView;
@property(nonatomic, strong) NCIIndexValueView *nciValue;
@property(nonatomic, strong) NCIIndexValueView *nepValue;
@property(nonatomic, strong) NCIIndexValueView *qpsValue;
@property(nonatomic, strong) NCIChartView *chartView;
@property(nonatomic, strong) UILabel *noConnectionLabel;

@end
