//
//  NCIEndpoint.m
//  Tapestry
//
//  Created by Ira on 5/23/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIEndpoint.h"
#import "NCIHintView.h"

@interface NCIEndpoint()

@end

@implementation NCIEndpoint

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        self.layer.cornerRadius = frame.size.width/2;
        UITapGestureRecognizer *tap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(showHint)];
        tap.numberOfTapsRequired = 1;
        [self addGestureRecognizer:tap];
    }
    return self;
}

- (void)showHint{
    CGPoint generalPoint = [self.superview convertPoint:self.center toView:[[[UIApplication sharedApplication] delegate] window].rootViewController.view];
    [[NCIHintView globaHint] showHintWithText: self.ip
                                     andPoint: CGPointMake(generalPoint.x + 10, generalPoint.y + 10)];
}

@end
