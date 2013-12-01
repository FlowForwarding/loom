//
//  NCIPeriodSwitcher.m
//  Tapestry
//
//  Created by Ira on 11/21/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIPeriodSwitcher.h"
#import "NCIWebSocketConnector.h"

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
    actionBlock();
    [[NCIWebSocketConnector interlocutor] requestLastDataForPeiodInSeconds:_period];
    self.backgroundColor = [UIColor grayColor];
    self.selected = YES;
}


@end
