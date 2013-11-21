//
//  NCIPeriodSwitcher.m
//  Tapestry
//
//  Created by Ira on 11/21/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIPeriodSwitcher.h"

@interface NCIPeriodSwitcher(){
    int period;
    void (^actionBlock)(void);
}

@end

@implementation NCIPeriodSwitcher


-(id)initWithFrame:(CGRect)frame label:(NSString *)label gap:(int) periodGap andAction:(void (^)(void))action{
    self = [self initWithFrame:frame];
    if (self){
        [self setTitle:label forState:UIControlStateNormal];
        period = periodGap;
        actionBlock = action;
        [self addTarget:self action:@selector(selectPeriod) forControlEvents:UIControlEventTouchUpInside];
    }
    return self;
};

-(void)deselect{
    self.backgroundColor = [UIColor whiteColor];
    self.selected = NO;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        [self setTitleColor:[UIColor blackColor] forState:UIControlStateNormal];
        [self setTitleColor:[UIColor whiteColor] forState:UIControlStateSelected];
        self.layer.cornerRadius = 20;
        self.layer.borderWidth = 1;
        self.layer.borderColor = [UIColor blackColor].CGColor;
    }
    return self;
}

- (void)selectPeriod{
    actionBlock();
    self.backgroundColor = [UIColor grayColor];
    self.selected = YES;
}


@end
