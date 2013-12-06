//
//  NCIHelpView.m
//  Tapestry
//
//  Created by Ira on 11/14/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIHelpView.h"

@interface NCIHelpView()<UIDocumentInteractionControllerDelegate>{
    UIButton *aboutLink;
    UIButton *nciLink;
    UIButton *ffLink;
    UIDocumentInteractionController *docController;
}

@end

int btnHeight = 40;
int buttonWidth = 300;

@implementation NCIHelpView

- (id)initIndependantly{
    self = [self initWithFrame:CGRectMake(0,  0, self.superview.frame.size.width, 0)];
    return self;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        self.backgroundColor = [UIColor colorWithWhite:0.1 alpha:0.2];
        
        aboutLink = [[UIButton alloc] init];
        [aboutLink setTitle: NSLocalizedString(@"About NCI", nil) forState:UIControlStateNormal];
        [self makeupLinkBtn: aboutLink];
        [aboutLink addTarget:self action:@selector(gotoAbout) forControlEvents:UIControlEventTouchUpInside];
        [self addSubview:aboutLink];
        
        nciLink = [[UIButton alloc] init];
        [nciLink setTitle: NSLocalizedString(@"NCI – Technical paper", nil) forState:UIControlStateNormal];
        [self makeupLinkBtn: nciLink];
        [nciLink addTarget:self action:@selector(gotoNCI) forControlEvents:UIControlEventTouchUpInside];
        [self addSubview:nciLink];
        
        ffLink = [[UIButton alloc] init];
        [ffLink setTitle: NSLocalizedString(@"About FlowForwarding.Org", nil) forState:UIControlStateNormal];
        [self makeupLinkBtn: ffLink];
        [ffLink addTarget:self action:@selector(gotoFlowForwarding) forControlEvents:UIControlEventTouchUpInside];
        [self addSubview:ffLink];
        
        self.backgroundColor = [UIColor colorWithWhite:0.1 alpha:0.2];
        [self initBittonFrames];
        
        UITapGestureRecognizer *tapBg = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(bgTapped)];
        self.userInteractionEnabled = YES;
        [self addGestureRecognizer:tapBg];
        
        
    }
    return self;
}

- (void)bgTapped{
    [self hideHelp];
}

- (void)makeupButton:(UIButton *)btn{
    [btn setTitleColor: [UIColor whiteColor] forState:UIControlStateNormal];
    btn.contentHorizontalAlignment = UIControlContentHorizontalAlignmentLeft;
    btn.backgroundColor = [UIColor colorWithWhite:0.3 alpha:1];
}

- (void)makeupLinkBtn:(UIButton *)btn{
    [self makeupButton:btn];
    btn.titleEdgeInsets = UIEdgeInsetsMake(0, 0, 0, 0);
    [btn setImage: [UIImage imageNamed:@"external_link"] forState:UIControlStateNormal];
    btn.imageEdgeInsets = UIEdgeInsetsMake(0 , [btn.titleLabel.text sizeWithFont:btn.titleLabel.font].width + 30, 0,0);
}


- (void)layoutSubviews{
    aboutLink.frame = CGRectMake(self.frame.size.width - buttonWidth,
                                 aboutLink.frame.origin.y, buttonWidth, aboutLink.frame.size.height);
    nciLink.frame = CGRectMake(self.frame.size.width - buttonWidth,
                               nciLink.frame.origin.y, buttonWidth, nciLink.frame.size.height);
    ffLink.frame = CGRectMake(self.frame.size.width - buttonWidth,
                              ffLink.frame.origin.y, buttonWidth, ffLink.frame.size.height);
}

- (void)gotoAbout{
    NSURL *url = [NSURL URLWithString:
                  @"http://www.infoblox.com/sites/infobloxcom/files/resources/infoblox-whitepaper-network-complexity.pdf"];
    [[UIApplication sharedApplication] openURL:url];
}

- (UIView *)documentInteractionControllerViewForPreview:(UIDocumentInteractionController *)controller{
    return self.superview;
}

- (void)gotoNCI{
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString: @"http://www.flowforwarding.org/nci-article"]];
}

- (void)gotoFlowForwarding{
    [[UIApplication sharedApplication] openURL:[NSURL URLWithString: @"http://www.flowforwarding.org/about-us"]];
}

- (void)hideHelp{
    [UIView animateWithDuration:0.3 animations:^{
        [self initBittonFrames];
    } completion:^(BOOL finished) {
        self.frame = CGRectMake(self.frame.origin.x, 0, self.frame.size.width, 0);
    }];
}

- (void)initBittonFrames{
    aboutLink.frame = CGRectMake(self.frame.size.width - buttonWidth, -3*btnHeight + 3, buttonWidth, 0);
    nciLink.frame = CGRectMake(self.frame.size.width - buttonWidth, -2*btnHeight + 2, buttonWidth, 0);
    ffLink.frame = CGRectMake(self.frame.size.width - buttonWidth, -btnHeight, buttonWidth, 0);
}

- (void)showHelp{
    if (self.frame.size.height != 0){
        [self hideHelp];
        return;
    };
    self.frame = self.superview.bounds;
    [UIView animateWithDuration:0.3 animations:^{
        aboutLink.frame = CGRectMake(self.frame.size.width - buttonWidth, 1, buttonWidth, btnHeight);
        nciLink.frame = CGRectMake(self.frame.size.width - buttonWidth, btnHeight + 2, buttonWidth, btnHeight);
        ffLink.frame = CGRectMake(self.frame.size.width - buttonWidth, 2*btnHeight + 3, buttonWidth, btnHeight);
    } completion:^(BOOL finished) {
        
    }];
}


@end
