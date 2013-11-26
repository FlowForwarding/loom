//
//  NCIPeriodSwitcherPanel.m
//  Tapestry
//
//  Created by Ira on 11/21/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIPeriodSwitcherPanel.h"
#import "NCIPeriodSwitcher.h"

@interface NCIPeriodSwitcherPanel (){
    NSMutableArray *buttons;
}

@end

@implementation NCIPeriodSwitcherPanel

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        buttons = [[NSMutableArray alloc] init];
        NSArray *periods = @[@[@"1h", [NSNumber numberWithInt:60*60]],
                             @[@"1d", [NSNumber numberWithInt:60*60*24]],
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
    return self;
}

-(void)resetButtons{
    for (NCIPeriodSwitcher *button in buttons){
        [button deselect];
    }
}



@end
