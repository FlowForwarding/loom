//
//  NCIGraphController.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/11/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIGraphController.h"
#import "NCIIndexValueView.h"
#import "NCIChartView.h"
#import "NCIHelpView.h"
#import "NCIEditServerView.h"
#import "NCIPeriodSwitcherPanel.h"
#import "NCIWebSocketConnector.h"

@interface NCIGraphController() <UIGestureRecognizerDelegate>{
    NCIIndexValueView *nciValue;
    NCIIndexValueView *nepValue;
    NCIIndexValueView *qpsValue;
    NCIChartView *chartView;
    UIButton *infoButton;
    NCIHelpView *helpView;

    UIImage *unavailableImage;
    UIButton *noConnection;
    NCIEditServerView *editServerView;
    NCIPeriodSwitcherPanel *switcherPanel;
    UILabel *progressLabel;
    
    bool isShowingLandscapeView;
}
@end


@implementation NCIGraphController

- (id)initWithNibName:(NSString *)nibNameOrNil bundle:(NSBundle *)nibBundleOrNil
{
    self = [super initWithNibName:nibNameOrNil bundle:nibBundleOrNil];
    if (self) {
        // Custom initialization
    }
    return self;
}

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    self.title = NSLocalizedString(@"Tapestry: A Network Complexity Analyzer", nil);
    UIBarButtonItem* editButton = [[UIBarButtonItem alloc]  initWithImage:[UIImage imageNamed:@"info"]
                                                                    style:UIBarButtonItemStyleBordered target:self action:@selector(showHelp)];
    [[self navigationItem] setRightBarButtonItem: editButton ];
    [self.navigationItem.rightBarButtonItem setTintColor:[UIColor whiteColor]];
    //for iOS 7 to make same calculations views y position
    if ([self respondsToSelector:@selector(edgesForExtendedLayout)]){
        self.edgesForExtendedLayout = UIRectEdgeNone;
        [self.navigationController.navigationBar setBarTintColor:[UIColor blackColor]];
        [self.navigationController.navigationBar setTitleTextAttributes:
         [NSDictionary dictionaryWithObjectsAndKeys:[UIColor whiteColor],
          UITextAttributeTextColor, nil]];
    } else {
        [self.navigationController.navigationBar setTintColor:[UIColor blackColor]];
    }
    
    switcherPanel = [[NCIPeriodSwitcherPanel alloc] initWithFrame:CGRectZero];
    [self.view addSubview:switcherPanel];
    
    
    nciValue = [[NCIIndexValueView alloc] initWithFrame:CGRectZero indName:NSLocalizedString(@"NCI", nil) indSize:22];
    [nciValue setTooltipText: NSLocalizedString(@"Network Complexity Index", nil)];
    
    [self.view addSubview:nciValue];
    qpsValue = [[NCIIndexValueView alloc] initWithFrame:CGRectZero
                                                indName:NSLocalizedString(@"Queries per Second", nil) indSize:14];
    
    [qpsValue setTooltipText:NSLocalizedString(@"Successful DNS Query Responses per Second", nil)];
    [self.view addSubview:qpsValue];
    
    nepValue = [[NCIIndexValueView alloc] initWithFrame:CGRectZero indName:NSLocalizedString(@"Endpoints", nil) indSize:14];
    [nepValue setTooltipText:NSLocalizedString(@"Number of Connected Network Elements", nil)];
    
    [self.view addSubview:nepValue];
    
    chartView = [[NCIChartView alloc] initWithFrame:CGRectZero];
    chartView.rangesMoved = ^(){
        [[NCIWebSocketConnector interlocutor].periodSwitcherPanel resetButtons];
    };
    [self.view addSubview:chartView];
    
    noConnection = [[UIButton alloc] initWithFrame:CGRectZero];
    unavailableImage = [UIImage imageNamed:@"unavailable"];
    [noConnection setImage:unavailableImage forState:UIControlStateNormal];
    [noConnection setHidden:YES];
    [noConnection addTarget:self action:@selector(reconnect) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview: noConnection];
    
    editServerView = [[NCIEditServerView alloc] initWithFrame:CGRectZero];
    [self.view addSubview:editServerView];
    
    helpView = [[NCIHelpView alloc] initIndependantly];
    [self.view addSubview:helpView];
    
    progressLabel = [[UILabel alloc] initWithFrame:CGRectZero];
    progressLabel.textColor = [UIColor tapestryDarkBlue];
    progressLabel.backgroundColor = [UIColor clearColor];
    progressLabel.textAlignment = NSTextAlignmentCenter;
    progressLabel.font = [UIFont boldSystemFontOfSize:20];
    progressLabel.hidden = YES;
    [progressLabel setText:NSLocalizedString(@"Loading...", nil)];
    [self.view addSubview:progressLabel];
    
    [self layoutSubviews];
    
    //TODO reorganize this
    [NCIWebSocketConnector interlocutor].editServerView = editServerView;
    [NCIWebSocketConnector interlocutor].nciValue = nciValue;
    [NCIWebSocketConnector interlocutor].nepValue = nepValue;
    [NCIWebSocketConnector interlocutor].qpsValue = qpsValue;
    [NCIWebSocketConnector interlocutor].chartView = chartView;
    [NCIWebSocketConnector interlocutor].noConnection =  noConnection;
    [NCIWebSocketConnector interlocutor].periodSwitcherPanel = switcherPanel;
    [NCIWebSocketConnector interlocutor].progressLabel = progressLabel;
    [[NCIWebSocketConnector interlocutor] reconnect];
    
    isShowingLandscapeView = NO;
    [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(orientationChanged:)
                                                 name:UIDeviceOrientationDidChangeNotification
                                               object:nil];
    [[UIApplication sharedApplication] setStatusBarStyle:UIStatusBarStyleLightContent];
    
    UITapGestureRecognizer *freeTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(freeTap)];
    freeTap.delegate = self;
    [self.view addGestureRecognizer:freeTap];
    
}

- (BOOL)gestureRecognizerShouldBegin:(UIGestureRecognizer *)gestureRecognizer{
    if (editServerView.active) {
        return NO;
    }
    return YES;
}

- (void)reconnect{
    [[NCIWebSocketConnector interlocutor] resetData];
}

- (void)freeTap{
    [[NSNotificationCenter defaultCenter] postNotificationName:@"freeTap" object:self];
}

- (void)showHelp{
    [helpView showHelp];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

- (void)layoutSubviews {
    int topIndent = 10;
    int indexLabelHeight = 50;
    if (isShowingLandscapeView) {
        
    } else {
        
    }
    
    float editHeigth = editServerView.frame.size.height == 0 ? 50 : editServerView.frame.size.height;
    editServerView.frame = CGRectMake(0, 0, self.view.bounds.size.width, editHeigth);
    
    nciValue.frame = CGRectMake(0, topIndent + indexLabelHeight, self.view.bounds.size.width/2, indexLabelHeight);
    
    qpsValue.frame = CGRectMake(self.view.bounds.size.width/2, topIndent + 2*indexLabelHeight + 25, self.view.bounds.size.width/2, indexLabelHeight);
    
    nepValue.frame = CGRectMake(self.view.bounds.size.width/2, indexLabelHeight + topIndent, self.view.bounds.size.width/2, indexLabelHeight);
    
    switcherPanel.frame  = CGRectMake(20, 200, 500, 40);
    
    noConnection.frame = CGRectMake((self.view.bounds.size.width - unavailableImage.size.width)/2, 250,
                                    unavailableImage.size.width,
                                    unavailableImage.size.height);
    
    chartView.frame = CGRectMake(0, 250, self.view.bounds.size.width, 430);
    
    infoButton.center = CGPointMake(self.view.bounds.size.width - 50, indexLabelHeight + 30);
    
    helpView.frame =  CGRectMake(0, helpView.frame.origin.y, self.view.frame.size.width, helpView.frame.size.height);
    
    progressLabel.frame = CGRectMake(0, 330, self.view.frame.size.width, 25);
}

- (void)orientationChanged:(NSNotification *)notification
{
    UIDeviceOrientation deviceOrientation = [UIDevice currentDevice].orientation;
    if (UIDeviceOrientationIsLandscape(deviceOrientation) &&
        !isShowingLandscapeView)
    {
        [self layoutSubviews];
        isShowingLandscapeView = YES;
    }
    else if (UIDeviceOrientationIsPortrait(deviceOrientation) &&
             isShowingLandscapeView)
    {
        [self layoutSubviews];
        isShowingLandscapeView = NO;
    }
}

@end
