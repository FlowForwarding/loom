//
//  NCIPeriodSwitcherPanel.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/21/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIPeriodSwitcherPanel.h"
#import "NCIPeriodSwitcher.h"
#import "NCIWebSocketConnector.h"

@interface NCIPeriodSwitcherPanel (){
    NSMutableArray *buttons;
    NSArray *periods;
}

@end

@implementation NCIPeriodSwitcherPanel

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        buttons = [[NSMutableArray alloc] init];
        periods = @[@[@"1d", [NSNumber numberWithInt:60*60*24]],
                             @[@"5d", [NSNumber numberWithInt:60*60*24*5]],
                             @[@"1m", [NSNumber numberWithInt:60*60*24*30]],
                             @[@"3m", [NSNumber numberWithInt:60*60*24*30*3]],
                             @[@"6m", [NSNumber numberWithInt:60*60*24*30*6]],
                             @[@"1y", [NSNumber numberWithInt:60*60*24*30*12]],
                             @[@"5y", [NSNumber numberWithInt:60*60*24*30*12*5]],
                             @[@"10y", [NSNumber numberWithInt:60*60*24*30*12*10]]
        ];
        int btnNum = 0;
        float btnSpace = 10;
        float btnWidth = 40;
        float btnHeight = 40;
        
        for (NSArray *switcherInfo in periods){
            __weak NCIPeriodSwitcherPanel* weakSelf = self;
            NCIPeriodSwitcher *switcher =
            [[NCIPeriodSwitcher alloc] initWithFrame:
             CGRectMake((btnSpace + btnWidth)*btnNum, 0, btnWidth, btnHeight)
                                               label:switcherInfo[0] gap:[switcherInfo[1] intValue] andAction:^{
                                                   [weakSelf resetButtons];
                                               }];
            [buttons addObject:switcher];
            [self addSubview:switcher];
            btnNum ++;
            
        };
        
    }
    [[NCIWebSocketConnector interlocutor] addObserver:self forKeyPath:@"startDate" options:NSKeyValueObservingOptionNew context:NULL];
    
    return self;
}

- (void)observeValueForKeyPath:(NSString *)keyPath
                      ofObject:(id)object
                        change:(NSDictionary *)change context:(void *)context{
    double diff = [[NSDate date] timeIntervalSince1970] - [[NCIWebSocketConnector interlocutor].startDate timeIntervalSince1970];
    NCIPeriodSwitcher *switcher;
    for (int i=0; i< buttons.count; i ++){
        switcher = buttons[i];
        if (switcher.period > diff && (i != 0) && (((NCIPeriodSwitcher *)buttons[i-1]).period > diff)){
            [switcher setEnabled:NO];
        } else {
            [switcher setEnabled:YES];
        }
    }
}

- (void)resetButtons{
    for (NCIPeriodSwitcher *button in buttons){
        [button deselect];
    }
}

@end
