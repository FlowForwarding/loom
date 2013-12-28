//
//  NCIPeriodSwitcher.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/21/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIPeriodSwitcher.h"
#import "NCIWebSocketConnector.h"

static float tenYearsPeriod = 60*60*24*30*12*10;

@interface NCIPeriodSwitcher(){
    void (^actionBlock)(void);
}

@end

@implementation NCIPeriodSwitcher


-(id)initWithFrame:(CGRect)frame label:(NSString *)label gap:(int) periodGap andAction:(void (^)(void))action{
    self = [self initWithFrame:frame];
    if (self){
        [self setTitle:label forState:UIControlStateNormal];
        _period = periodGap;
        actionBlock = action;
        [self addTarget:self action:@selector(selectPeriod) forControlEvents:UIControlEventTouchUpInside];
    }
    return self;
};

-(void)deselect{
    if (self.enabled){
        self.backgroundColor = [UIColor whiteColor];
        self.selected = NO;
        self.enabled = YES;
    }
}

- (void)setEnabled:(BOOL)enabled{
    [super setEnabled:enabled];
    if (enabled){
        self.backgroundColor = [UIColor whiteColor];
        self.layer.borderColor = [UIColor blackColor].CGColor;
    } else {
        self.backgroundColor = [UIColor colorWithWhite:0.9 alpha:1];
        self.layer.borderColor = [UIColor grayColor].CGColor;
    }
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        [self setTitleColor:[UIColor blackColor] forState:UIControlStateNormal];
        [self setTitleColor:[UIColor whiteColor] forState:UIControlStateSelected];
        [self setTitleColor:[UIColor grayColor] forState:UIControlStateDisabled];
        self.layer.cornerRadius = 20;
        self.layer.borderWidth = 1;
        self.layer.borderColor = [UIColor blackColor].CGColor;
    }
    return self;
}

- (void)selectPeriod{
    if ([NCIWebSocketConnector interlocutor].chartView.chartData.count < 2 )
        return;
    
    actionBlock();
    
    [NCIWebSocketConnector interlocutor].chartView.maxRangeVal =
     [[[NCIWebSocketConnector interlocutor].chartView.chartData lastObject][0] timeIntervalSince1970];
    double newMin = [NCIWebSocketConnector interlocutor].chartView.maxRangeVal  - _period ;
    if (([NCIWebSocketConnector interlocutor].currentDatePeriod == twoYearPeriod && _period <= twoYearPeriod) ||
        ([NCIWebSocketConnector interlocutor].currentDatePeriod == tenYearsPeriod && _period > twoYearPeriod)){
        if (newMin < [[NCIWebSocketConnector interlocutor].chartView.chartData[0][0] timeIntervalSince1970]){
            newMin = [[NCIWebSocketConnector interlocutor].chartView.chartData[0][0] timeIntervalSince1970];
        }
        [NCIWebSocketConnector interlocutor].chartView.minRangeVal=  newMin;
        [[NCIWebSocketConnector interlocutor].chartView drawChart];
    } else {
        if (newMin < [[NCIWebSocketConnector interlocutor].startDate timeIntervalSince1970]){
            newMin = [[NCIWebSocketConnector interlocutor].startDate timeIntervalSince1970];
        }
        [NCIWebSocketConnector interlocutor].chartView.minRangeVal =  newMin;
        [[NCIWebSocketConnector interlocutor].chartView resetChart];
        [[NCIWebSocketConnector interlocutor].chartView drawChart];
        long period = (_period <= twoYearPeriod) ? twoYearPeriod : tenYearsPeriod;
        [NCIWebSocketConnector interlocutor].currentDatePeriod = period;
        [[NCIWebSocketConnector interlocutor] requestLastDataForPeiodInSeconds:period];
    }
    self.backgroundColor = [UIColor grayColor];
    self.selected = YES;
}


@end
