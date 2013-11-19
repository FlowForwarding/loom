//
//  NCIEditServerView.m
//  Tapestry
//
//  Created by Ira on 11/18/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIEditServerView.h"

@protocol PresentNCIProtocol <NSObject>

-(void)resetData;

@end

@interface NCIEditServerView()<UITextFieldDelegate>{
    UIButton *actionsBtn;
    UIButton *connectUrlBtn;
    UIButton *cancelBtn;
    UIButton *defaultBtn;
    UITextField *serverUrlEdit;
    id<PresentNCIProtocol> actionsTarget;
    
    NSString *savedUrl;
    bool actionsShown;
}

@end

static NSString* defaultWebsocketUrl = @"ws://nci.ilabs.inca.infoblox.com:28080/clientsock.yaws";
static int editServerInputHeigth = 40;
static int btnWidth = 100;
static int btnHeigth = 38;

@implementation NCIEditServerView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        savedUrl = defaultWebsocketUrl;
        actionsShown = NO;
        
        serverUrlEdit = [[UITextField alloc] initWithFrame:CGRectZero];
        serverUrlEdit.backgroundColor = [UIColor colorWithRed:246/255.0 green:246/255.0 blue:246/255.0 alpha:1.0];
        serverUrlEdit.layer.borderColor = [UIColor colorWithRed:246/255.0 green:246/255.0 blue:246/255.0 alpha:1.0].CGColor;
        serverUrlEdit.layer.borderWidth = 1.0;
        serverUrlEdit.text = defaultWebsocketUrl;
        serverUrlEdit.layer.cornerRadius = 10;
        serverUrlEdit.contentVerticalAlignment = UIControlContentVerticalAlignmentCenter;
        serverUrlEdit.delegate = self;
        UITextView *serverEditLeftView = [[UITextView alloc] initWithFrame:CGRectMake(0, 0, 145, editServerInputHeigth)];
        serverEditLeftView.backgroundColor =  [UIColor colorWithRed:246/255.0 green:246/255.0 blue:246/255.0 alpha:1.0];
        serverEditLeftView.text = NSLocalizedString(@"Tapestry Server", nil);
        serverEditLeftView.font = [UIFont boldSystemFontOfSize:16];
        serverEditLeftView.textColor = [UIColor blackColor];
        serverUrlEdit.leftView = serverEditLeftView;
        serverUrlEdit.leftViewMode = UITextFieldViewModeAlways;
        [self addSubview:serverUrlEdit];
        
        connectUrlBtn = [[UIButton alloc] initWithFrame:CGRectMake(self.bounds.size.width - 110, 0, btnWidth, btnHeigth)];
        [connectUrlBtn setTitle: NSLocalizedString(@"Connect", nil) forState:UIControlStateNormal];
        [connectUrlBtn setTitleColor:[UIColor blackColor] forState:UIControlStateNormal];
        [connectUrlBtn addTarget:self action:@selector(connectlUrl) forControlEvents:UIControlEventTouchUpInside];
        connectUrlBtn.backgroundColor = [UIColor colorWithWhite:0 alpha:0.3];
        [self addSubview:connectUrlBtn];
        
        cancelBtn = [[UIButton alloc] initWithFrame:CGRectMake(self.bounds.size.width - 110, 0, btnWidth, btnHeigth)];
        [cancelBtn setTitle: NSLocalizedString(@"Cancel", nil) forState:UIControlStateNormal];
        [cancelBtn addTarget:self action:@selector(cancelUrlChanges) forControlEvents:UIControlEventTouchUpInside];
        [cancelBtn setTitleColor:[UIColor blackColor] forState:UIControlStateNormal];
        cancelBtn.backgroundColor = [UIColor colorWithWhite:0 alpha:0.3];
        [self addSubview:cancelBtn];
        
        defaultBtn = [[UIButton alloc] initWithFrame:CGRectMake(self.bounds.size.width - 110, 0, btnWidth, btnHeigth)];
        [defaultBtn setTitle: NSLocalizedString(@"Default", nil) forState:UIControlStateNormal];
        [defaultBtn addTarget:self action:@selector(setDefaultUrl) forControlEvents:UIControlEventTouchUpInside];
        [defaultBtn setTitleColor:[UIColor blackColor] forState:UIControlStateNormal];
        defaultBtn.backgroundColor = [UIColor colorWithWhite:0 alpha:0.3];
        [self addSubview:defaultBtn];
        
        actionsBtn  = [[UIButton alloc] initWithFrame:CGRectMake(self.bounds.size.width - 110, 0, btnWidth, btnHeigth)];
        [actionsBtn setTitle: NSLocalizedString(@"Actions", nil) forState:UIControlStateNormal];
        [actionsBtn addTarget:self action:@selector(toggleActions) forControlEvents:UIControlEventTouchUpInside];
        [actionsBtn setTitleColor:[UIColor whiteColor] forState:UIControlStateNormal];
        actionsBtn.backgroundColor = [UIColor colorWithWhite:0 alpha:0.8];
        [self addSubview:actionsBtn];
        UITapGestureRecognizer *tapBg = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapBg)];
        self.userInteractionEnabled = YES;
        [self addGestureRecognizer:tapBg];
        tapBg.numberOfTapsRequired = 1;
        
    }
    return self;
}

- (void)tapBg{
    [serverUrlEdit resignFirstResponder];
}

- (BOOL)textFieldShouldBeginEditing:(UITextField *)textField{
    textField.backgroundColor = [UIColor whiteColor];
    [self showActions];
    return YES;
}

- (void)textFieldDidEndEditing:(UITextField *)textField{
    textField.backgroundColor =  [UIColor colorWithRed:246/255.0 green:246/255.0 blue:246/255.0 alpha:1.0];
    [self hideActions];
}

- (void)toggleActions{
    if (!actionsShown){
        [self showActions];
        if (![serverUrlEdit isFirstResponder]){
            [serverUrlEdit  becomeFirstResponder];
        };
    } else {
        [self hideActions];
    }
}

- (void)showActions{
    actionsShown = YES;
    [UIView animateWithDuration:0.3 animations:^{
        connectUrlBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, 40 + btnHeigth/2);
        defaultBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, 80 + btnHeigth/2);
        cancelBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, 120 + btnHeigth/2);
    }];
}

- (void)hideActions{
    actionsShown = NO;
    [serverUrlEdit resignFirstResponder];
    [UIView animateWithDuration:0.3 animations:^{
        connectUrlBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, 0 + btnHeigth/2);
        defaultBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, 0 + btnHeigth/2);
        cancelBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, 0 + btnHeigth/2);
    }];
}

-(void)connectlUrl{
    savedUrl = serverUrlEdit.text;
    NSLog(@"%@", actionsTarget);
    [actionsTarget resetData];
    [self hideActions];
}

-(void)cancelUrlChanges{
    serverUrlEdit.text = savedUrl;
    [self hideActions];
}

-(void)setDefaultUrl{
    serverUrlEdit.text = defaultWebsocketUrl;
    savedUrl = defaultWebsocketUrl;
    [actionsTarget resetData];
    [self hideActions];
}

-(NSString *)getServerUrl{
    return serverUrlEdit.text;
}

-(id)initWithTarget:(id)target{
    self = [self initWithFrame:CGRectZero];
    if (self){
        actionsTarget = target;
    }
    return self;
}

- (void)layoutSubviews {
    serverUrlEdit.frame = CGRectMake(10, 0, self.bounds.size.width - 130, editServerInputHeigth);
    actionsBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, actionsBtn.center.y);
    connectUrlBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, connectUrlBtn.center.y);
    defaultBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, defaultBtn.center.y);
    cancelBtn.center = CGPointMake(self.bounds.size.width - 110 + btnWidth/2, cancelBtn.center.y);

}


@end
