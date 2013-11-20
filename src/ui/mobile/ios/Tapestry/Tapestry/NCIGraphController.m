//
//  NCIGraphController.m
//  Tapestry
//
//  Created by Ira on 11/11/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGraphController.h"
#import "SRWebSocket.h"
#import "NCIIndexValueView.h"
#import "NCIChartView.h"
#import "NCIHelpView.h"
#import "NCIEditServerView.h"

@interface NCIGraphController() <SRWebSocketDelegate>{
    SRWebSocket *socket;
    NCIIndexValueView *nciValue;
    NCIIndexValueView *nepValue;
    NCIIndexValueView *qpsValue;
    NCIChartView *graphView;
    UIButton *infoButton;
    NCIHelpView *helpView;

    UILabel *noConnectionLabel;
    NSDateFormatter *serverDateformatter;
    NCIEditServerView *editServerView;
    
    bool isShowingLandscapeView;
    
    int timeAdjustment;
    
    
   // NSString *websocketUrl;
}
@end


static NSString* websocketStartRequest = @"START_DATA";
static NSString* websocketMoreDataRequest =
    @"{\"request\":\"more_data\",\"start\": \"%@Z\",\"end\": \"%@Z\",\"max_items\": \"300\"}";

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
    UIBarButtonItem* editButton = [[UIBarButtonItem alloc]  initWithImage:[UIImage imageNamed:@"actionsarrow"]
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
    
    graphView = [[NCIChartView alloc] initWithFrame:CGRectZero];
    graphView.backgroundColor = [UIColor yellowColor];
    [self.view addSubview:graphView];
    
    editServerView = [[NCIEditServerView alloc] initWithTarget:self];
    [self.view addSubview:editServerView];
    
    noConnectionLabel = [[UILabel alloc] initWithFrame:CGRectZero];
    noConnectionLabel.text = NSLocalizedString(@"Can't connect, please try agian.", nil);
    noConnectionLabel.backgroundColor = [UIColor clearColor];
    noConnectionLabel.font = [UIFont boldSystemFontOfSize:22];
    noConnectionLabel.textAlignment = NSTextAlignmentCenter;
    noConnectionLabel.textColor = [UIColor redColor];
    [noConnectionLabel setHidden:YES];
    [self.view addSubview:noConnectionLabel];
    
    helpView = [[NCIHelpView alloc] initWithFrame:self.view.bounds];
    [self.view addSubview:helpView];
    
    [self layoutSubviews];
    
    [self reconnect];
    
    serverDateformatter = [[NSDateFormatter alloc] init];
    [serverDateformatter setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
    [serverDateformatter setDateFormat:@"yyyy-MM-dd_HH:mm:ss"];
    
    isShowingLandscapeView = NO;
    [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(orientationChanged:)
                                                 name:UIDeviceOrientationDidChangeNotification
                                               object:nil];
    [[UIApplication sharedApplication] setStatusBarStyle:UIStatusBarStyleLightContent];
    
}

- (void)resetData{
    [nciValue resetData];
    [qpsValue resetData];
    [nepValue resetData];
    [noConnectionLabel setHidden:YES];
    [graphView resetChart];
    [self reconnect];
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
    
    editServerView.frame = CGRectMake(0, topIndent, self.view.bounds.size.width, 170);
    
    nciValue.frame = CGRectMake(0, 2*topIndent + indexLabelHeight, self.view.bounds.size.width/2, indexLabelHeight);
    
    qpsValue.frame = CGRectMake(self.view.bounds.size.width/2, 2*topIndent + 2*indexLabelHeight + 25, self.view.bounds.size.width/2, indexLabelHeight);
    
    nepValue.frame = CGRectMake(self.view.bounds.size.width/2, indexLabelHeight + 2*topIndent, self.view.bounds.size.width/2, indexLabelHeight);
    
    noConnectionLabel.frame = CGRectMake(0, 200, self.view.bounds.size.width, 50);
    
    graphView.frame = CGRectMake(0, 200, self.view.bounds.size.width, 450);
    
    infoButton.center = CGPointMake(self.view.bounds.size.width - 50, indexLabelHeight + 30);
    
    helpView.frame = self.view.bounds;
}

- (void)reconnect
{
    socket.delegate = nil;
    [socket close];
    socket = [[SRWebSocket alloc] initWithURLRequest: [NSURLRequest requestWithURL: [NSURL URLWithString: [editServerView getServerUrl]]]];
    socket.delegate = self;
    [socket open];
    
}

#pragma mark - SRWebSocketDelegate

- (void)webSocketDidOpen:(SRWebSocket *)webSocket;
{
    NSLog(@"Websocket Connected");
    [webSocket send:websocketStartRequest];
}

- (void)webSocket:(SRWebSocket *)webSocket didFailWithError:(NSError *)error;
{
    NSLog(@"Websocket Failed With Error %@", error);
    [noConnectionLabel setHidden:NO];
    webSocket = nil;
}

- (void)webSocket:(SRWebSocket *)webSocket didReceiveMessage:(id)message;
{
    NSString *messageString = ((NSString *)message);
    NSArray *dataPieces = [[messageString substringWithRange:NSMakeRange(1, messageString.length -2) ] componentsSeparatedByString:@","];
    if (dataPieces.count > 2){
        int i;
        for (i = 0; i < dataPieces.count/2; i+=2){
            //we get such fromat data 2013-11-12T14:04:29Z
            NSString *dateString = [dataPieces[i*2] substringWithRange:NSMakeRange(8, ((NSString *)dataPieces[i]).length - 10)];
            dateString = [dateString stringByReplacingOccurrencesOfString:@"T" withString:@"_"];
            NSDate *date = [serverDateformatter dateFromString:dateString];
            NSString *nciVal = [dataPieces[2*i+1] substringFromIndex:6];
            [graphView addPoint:date val:nciVal];
        }
        [graphView drawChart];
        
    } else {
        NSDictionary *dataPoint = [NSJSONSerialization
                                   JSONObjectWithData:[message dataUsingEncoding:NSUTF8StringEncoding]
                                   options:NSJSONReadingMutableContainers error:NULL];
    
        NSString *nci = dataPoint[@"NCI"];
        NSString *nep = dataPoint[@"NEP"];
        NSString *qps = dataPoint[@"QPS"];
        if (nci){
            [nciValue setIndValue:nci withDate:dataPoint[@"Time"]];
        } else if (nep) {
            [nepValue setIndValue:nep  withDate:dataPoint[@"Time"]];
        } else if (qps) {
            [qpsValue setIndValue:qps withDate:dataPoint[@"Time"]];
        }
        //{"start_time":"2013-11-13T16:42:55Z","current_time":"2013-11-13T21:23:47Z"}
        NSString *start_time = dataPoint[@"start_time"];
        if (start_time){
            NSString *current_time = dataPoint[@"current_time"];
            current_time = [current_time stringByReplacingOccurrencesOfString:@"T" withString:@"_"];
            NSDate *date = [serverDateformatter dateFromString:current_time];
            timeAdjustment = [date timeIntervalSinceNow];
            
            NSDate *endDate = [[NSDate date] dateByAddingTimeInterval: -timeAdjustment];
            NSDate *startDate = [[[NSDate date] dateByAddingTimeInterval: -timeAdjustment]
                                 dateByAddingTimeInterval: -60*60*24];
            [graphView setMinX:startDate];
            [graphView setMaxX:endDate];
            [graphView setRanges:[endDate dateByAddingTimeInterval: -60*60*2] max:endDate];
            
            NSString *endDateString = [[serverDateformatter stringFromDate: endDate]
                                 stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
            NSString *startDateString = [[serverDateformatter stringFromDate:startDate]
                                   stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
            [webSocket send: [NSString stringWithFormat: websocketMoreDataRequest, startDateString, endDateString]];
        }
    }
}

- (void)webSocket:(SRWebSocket *)webSocket didCloseWithCode:(NSInteger)code reason:(NSString *)reason wasClean:(BOOL)wasClean;
{
    NSLog(@"WebSocket closed");
    [noConnectionLabel setHidden:NO];
    socket = nil;
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
