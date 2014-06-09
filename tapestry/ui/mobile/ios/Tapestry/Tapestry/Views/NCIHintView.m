//
//  NCIHintView.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/25/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIHintView.h"

@interface NCIHintView(){}
@property(nonatomic, strong)UILabel *hintLabel;
@end

@implementation NCIHintView

+ (id)globaHint{
    static dispatch_once_t single;
    static NCIHintView *globaHint;
    dispatch_once(&single, ^ {
        UIFont *textFont = [UIFont systemFontOfSize:16];
        globaHint = [[self alloc] initWithFrame:[[[UIApplication sharedApplication] delegate] window].rootViewController.view.bounds];
        globaHint.hintLabel = [[UILabel alloc] initWithFrame: CGRectZero];
        globaHint.hintLabel.numberOfLines = 0;
        globaHint.hintLabel.lineBreakMode = NSLineBreakByWordWrapping;
        globaHint.hintLabel.font = textFont;
        globaHint.hintLabel.textColor = [UIColor whiteColor];
        globaHint.hintLabel.layer.backgroundColor = [UIColor colorWithWhite:0.1 alpha:0.9].CGColor;
        globaHint.hintLabel.layer.cornerRadius = 10.0;
        globaHint.hintLabel.layer.borderColor = [UIColor blackColor].CGColor;
        globaHint.hintLabel.textAlignment = NSTextAlignmentCenter;
        globaHint.hintLabel.layer.borderWidth = 1.0;
        [globaHint addSubview:globaHint.hintLabel];
        globaHint.hintLabel.backgroundColor = [UIColor clearColor];
        globaHint.backgroundColor =  [UIColor colorWithWhite:0.1 alpha:0.1];
        [[[[UIApplication sharedApplication] delegate] window].rootViewController.view addSubview:globaHint];
        globaHint.hidden = YES;
        UITapGestureRecognizer *tap = [[UITapGestureRecognizer alloc] initWithTarget:globaHint action:@selector(hideHint)];
        tap.numberOfTapsRequired = 1;
        [globaHint addGestureRecognizer:tap];
    });
    return globaHint;
}

- (void)showHintWithText:(NSString *)text andPoint:(CGPoint)point{
    UIFont *textFont = [UIFont systemFontOfSize:16];
    CGSize labelSize = [text sizeWithFont:textFont
                        constrainedToSize:CGSizeMake(500, 500) lineBreakMode:NSLineBreakByWordWrapping];
    self.hintLabel.frame = CGRectMake(point.x, point.y, 100 + labelSize.width, 50 + labelSize.height);
    self.hintLabel.text = text;
    self.hidden = NO;
}

- (void)hideHint{
    self.hidden = YES;
}



@end
