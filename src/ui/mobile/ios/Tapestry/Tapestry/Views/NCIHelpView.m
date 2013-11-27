//
//  NCIHelpView.m
//  Tapestry
//
//  Created by Ira on 11/14/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIHelpView.h"

@interface NCIHelpView(){
    int topViewHeigth;
    int shadowHeigth;
    UIButton *aboutLink;
    UIButton *nciLink;
    UIButton *ffLink;
}

@end

int btnHeight = 40;

@implementation NCIHelpView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        topViewHeigth  = 140;
        shadowHeigth = 1;
        self.backgroundColor = [UIColor colorWithWhite:0.1 alpha:0.2];
        self.hidden = YES;
        
        aboutLink = [[UIButton alloc] initWithFrame:CGRectZero];
        [aboutLink setTitle: NSLocalizedString(@"About NCI", nil) forState:UIControlStateNormal];
        [self makeupButton: aboutLink];
        [aboutLink addTarget:self action:@selector(gotoAbout) forControlEvents:UIControlEventTouchUpInside];
        [self addSubview:aboutLink];
        
        nciLink = [[UIButton alloc] initWithFrame:CGRectZero];
        [nciLink setTitle: NSLocalizedString(@"NCI – Technical paper", nil) forState:UIControlStateNormal];
        [self makeupButton: nciLink];
        [nciLink addTarget:self action:@selector(gotoNCI) forControlEvents:UIControlEventTouchUpInside];
        [self addSubview:nciLink];
        
        ffLink = [[UIButton alloc] initWithFrame:CGRectZero];
        [ffLink setTitle: NSLocalizedString(@"About FlowForwarding.Org", nil) forState:UIControlStateNormal];
        [self makeupButton: ffLink];
        [ffLink addTarget:self action:@selector(gotoFlowForwarding) forControlEvents:UIControlEventTouchUpInside];
        [self addSubview:ffLink];
        
        self.backgroundColor = [UIColor clearColor];
    }
    return self;
}

- (void)makeupButton:(UIButton *)btn{
    [btn setTitleColor: [UIColor whiteColor] forState:UIControlStateNormal];
    btn.backgroundColor = [UIColor colorWithWhite:0.3 alpha:1];
    [btn setImage: [UIImage imageNamed:@"external_link"] forState:UIControlStateNormal];
    btn.imageEdgeInsets = UIEdgeInsetsMake(0, 0, 0, -2*[btn.titleLabel.text sizeWithFont:btn.titleLabel.font].width - 60);
    
}

- (void)layoutSubviews{
    aboutLink.frame = CGRectMake(0, 1, self.frame.size.width, btnHeight);
    nciLink.frame = CGRectMake(0, btnHeight + 2, self.frame.size.width, btnHeight);
    ffLink.frame = CGRectMake(0, 2*btnHeight + 3, self.frame.size.width, btnHeight);
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
        self.frame = CGRectMake(0, -(btnHeight*3+3), self.frame.size.width, 0);
    } completion:^(BOOL finished) {
        self.hidden = YES;
    }];
}

- (void)showHelp{
    if (!self.hidden){
        [self hideHelp];
        return;
    }
    [UIView animateWithDuration:0.3 animations:^{
        self.hidden = NO;
        self.frame = CGRectMake(0, 0, self.frame.size.width, btnHeight*3+3);
    }];
}


@end
