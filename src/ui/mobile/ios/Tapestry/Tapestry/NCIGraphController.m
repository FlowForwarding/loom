//
//  NCIGraphController.m
//  Tapestry
//
//  Created by Ira on 11/11/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGraphController.h"
#import "NCIIndexValueView.h"
#import "NCIChartView.h"
#import "NCIHelpView.h"
#import "NCIEditServerView.h"
#import "NCIPeriodSwitcherPanel.h"
#import "NCIWebSocketConnector.h"

@interface NCIGraphController(){
    NCIIndexValueView *nciValue;
    NCIIndexValueView *nepValue;
    NCIIndexValueView *qpsValue;
    NCIChartView *chartView;
    UIButton *infoButton;
    NCIHelpView *helpView;

    UIImageView *noConnection;
    NCIEditServerView *editServerView;
    NCIPeriodSwitcherPanel *switcherPanel;
    
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
    [self.view addSubview:chartView];
    
    editServerView = [[NCIEditServerView alloc] initWithFrame:CGRectZero];
    [self.view addSubview:editServerView];
    
    noConnection = [[UIImageView alloc] initWithFrame:CGRectZero];
    noConnection.image = [UIImage imageNamed:@"unavailable"];
    [noConnection setHidden:YES];
    [self.view addSubview: noConnection];
    
    helpView = [[NCIHelpView alloc] initIndependantly];
    [self.view addSubview:helpView];
    
    [self layoutSubviews];
    
    //TODO reorganize this
    [NCIWebSocketConnector interlocutor].editServerView = editServerView;
    [NCIWebSocketConnector interlocutor].nciValue = nciValue;
    [NCIWebSocketConnector interlocutor].nepValue = nepValue;
    [NCIWebSocketConnector interlocutor].qpsValue = qpsValue;
    [NCIWebSocketConnector interlocutor].chartView = chartView;
    [NCIWebSocketConnector interlocutor].noConnection =  noConnection;
    [NCIWebSocketConnector interlocutor].periodSwitcherPanel = switcherPanel;
    [[NCIWebSocketConnector interlocutor] reconnect];
    
    isShowingLandscapeView = NO;
    [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(orientationChanged:)
                                                 name:UIDeviceOrientationDidChangeNotification
                                               object:nil];
    [[UIApplication sharedApplication] setStatusBarStyle:UIStatusBarStyleLightContent];
    
    UITapGestureRecognizer *freeTap = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(freeTap)];
    [self.view addGestureRecognizer:freeTap];
    
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
    
    float editHeigth = editServerView.frame.size.height == 0 ? 40 : editServerView.frame.size.height;
    editServerView.frame = CGRectMake(0, topIndent, self.view.bounds.size.width, editHeigth);
    
    nciValue.frame = CGRectMake(0, 2*topIndent + indexLabelHeight, self.view.bounds.size.width/2, indexLabelHeight);
    
    qpsValue.frame = CGRectMake(self.view.bounds.size.width/2, 2*topIndent + 2*indexLabelHeight + 25, self.view.bounds.size.width/2, indexLabelHeight);
    
    nepValue.frame = CGRectMake(self.view.bounds.size.width/2, indexLabelHeight + 2*topIndent, self.view.bounds.size.width/2, indexLabelHeight);
    
    switcherPanel.frame  = CGRectMake(20, 200, 500, 40);
    
    noConnection.frame = CGRectMake((self.view.bounds.size.width - noConnection.image.size.width)/2, 250,
                                    noConnection.image.size.height,
                                    noConnection.image.size.width);
    
    chartView.frame = CGRectMake(0, 250, self.view.bounds.size.width, 430);
    
    infoButton.center = CGPointMake(self.view.bounds.size.width - 50, indexLabelHeight + 30);
    
    helpView.frame =  CGRectMake(0, helpView.frame.origin.y, self.view.frame.size.width, helpView.frame.size.height);
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
