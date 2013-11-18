//
//  NCIHelpView.m
//  Tapestry
//
//  Created by Ira on 11/14/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIHelpView.h"

@interface NCIHelpView(){
    UIView *topView;
    UIView *shadowView;
    int topViewHeigth;
    int shadowHeigth;
    CAGradientLayer * gradient;
}

@end

@implementation NCIHelpView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        topViewHeigth  = 140;
        shadowHeigth = 3;
        self.backgroundColor = [UIColor colorWithWhite:0.1 alpha:0.2];
        self.hidden = YES;
        
        topView = [[UIView alloc] initWithFrame:CGRectMake(0, -topViewHeigth, self.frame.size.width, topViewHeigth)];
        topView.backgroundColor = [UIColor whiteColor];
        [self addSubview:topView];
        
        shadowView = [[UIView alloc] initWithFrame:CGRectMake(0, topViewHeigth - shadowHeigth, topView.frame.size.width, shadowHeigth)];
        [topView addSubview:shadowView];
        
        gradient = [CAGradientLayer layer];
        [gradient setFrame:[shadowView bounds]];
        [gradient setColors:[NSArray arrayWithObjects:(id)[UIColor whiteColor].CGColor, (id)[UIColor colorWithWhite:0.2 alpha:1].CGColor, nil]];
        [shadowView.layer addSublayer:gradient];
        
        int leftIndent =  20;
        int labelWidth = 300;
        
        UILabel *aboutLink = [[UILabel alloc] initWithFrame:CGRectMake(leftIndent, 20, labelWidth,  20)];
        aboutLink.text = NSLocalizedString(@"About NCI", nil);
        aboutLink.textColor = [UIColor blueColor];
        [topView addSubview:aboutLink];
        aboutLink.userInteractionEnabled = YES;
        UITapGestureRecognizer *tapAbout = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(gotoAbout)];
        tapAbout.numberOfTapsRequired = 1;
        [aboutLink addGestureRecognizer:tapAbout];
        
        UILabel *nciLink = [[UILabel alloc] initWithFrame:CGRectMake(leftIndent, 50, labelWidth, 20)];
        nciLink.text = NSLocalizedString(@"NCI – Technical paper", nil);
        [topView addSubview:nciLink];
        nciLink.userInteractionEnabled = YES;
        nciLink.textColor = [UIColor blueColor];
        UITapGestureRecognizer *tapNCI = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(gotoNCI)];
        tapNCI.numberOfTapsRequired = 1;
        [nciLink addGestureRecognizer:tapNCI];
        
        UILabel *ffLink = [[UILabel alloc] initWithFrame:CGRectMake(leftIndent, 80, labelWidth, 20)];
        ffLink.text = NSLocalizedString(@"About FlowForwarding.Org", nil);
        [topView addSubview:ffLink];
        ffLink.userInteractionEnabled = YES;
        ffLink.textColor = [UIColor blueColor];
        UITapGestureRecognizer *tapFF = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(gotoFlowForwarding)];
        tapFF.numberOfTapsRequired = 1;
        [ffLink addGestureRecognizer:tapFF];

        UITapGestureRecognizer *hideTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(hideHelp)];
        hideTap.numberOfTapsRequired = 1;
        self.userInteractionEnabled = YES;
        [self addGestureRecognizer:hideTap];
    }
    return self;
}

- (void)layoutSubviews{
    topView.frame = CGRectMake(0, topView.frame.origin.y, self.frame.size.width, topViewHeigth);
    shadowView.frame = CGRectMake(0, topViewHeigth - shadowHeigth, topView.frame.size.width, shadowHeigth);
    [gradient setFrame:[shadowView bounds]];
}

- (void)gotoAbout{
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString: @"http://www.infoblox.com"]];
}

- (void)gotoNCI{
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString: @"http://www.flowforwarding.org/nci"]];
}

- (void)gotoFlowForwarding{
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString: @"http://www.flowforwarding.org/about-us"]];
}

- (void)hideHelp{
    [UIView animateWithDuration:0.3 animations:^{
        topView.frame = CGRectMake(0, -topViewHeigth, self.frame.size.width, 0);
    } completion:^(BOOL finished) {
        self.hidden = YES;
    }];
}

- (void)showHelp{

    [UIView animateWithDuration:0.3 animations:^{
        self.hidden = NO;
        topView.frame = CGRectMake(0, 0, self.frame.size.width, topViewHeigth);
    }];
}


@end
