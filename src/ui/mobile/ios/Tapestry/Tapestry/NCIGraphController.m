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

@interface NCIGraphController() <SRWebSocketDelegate>{
    SRWebSocket *socket;
    NCIIndexValueView *nciValue;
    NCIIndexValueView *nepValue;
    NCIIndexValueView *qpsValue;
    NCIChartView *graphView;
    UIButton *infoButton;
    NCIHelpView *helpView;
    
    NSDateFormatter *serverDateformatter;
    bool isShowingLandscapeView;
    
    int timeAdjustment;
}
@end

static NSString* websocketUrl = @"ws://nci.ilabs.inca.infoblox.com:28080/clientsock.yaws";
static NSString* websocketStartRequest = @"START_DATA";
static NSString* websocketMoreDataRequest =
    @"{\"request\":\"more_data\",\"start\": \"%@Z\",\"end\": \"%@Z\",\"max_items\": \"20\"}";

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
    //for iOS 7 to make same calculations views y position
    if ([self respondsToSelector:@selector(edgesForExtendedLayout)]){
        self.edgesForExtendedLayout = UIRectEdgeNone;
    };
    
    self.title = NSLocalizedString(@"Tapestry: A Network Complexity Analyzer", nil);
    
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
    graphView.backgroundColor = [UIColor whiteColor];
    [self.view addSubview:graphView];
    
    infoButton = [[UIButton alloc] initWithFrame:CGRectMake(self.view.bounds.size.width - 50, 20, 30, 30)];
    infoButton.backgroundColor = [UIColor blackColor];
    infoButton.layer.cornerRadius = 15;
    [infoButton setTitle:@"i" forState:UIControlStateNormal];
    infoButton.titleLabel.font = [UIFont boldSystemFontOfSize:24];
    [infoButton addTarget:self action:@selector(showHelp) forControlEvents:UIControlEventTouchUpInside];
    [self.view addSubview:infoButton];
    
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

- (void)showHelp{
   // if(helpView.isPresented){
//    [UIView animateWithDuration:0.3 animations:^{
//        helpView.frame = CGRectMake(0, 0, self.view.frame.size.width, 100);
//    }];
    [helpView showHelp];
   // }
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
    
    nciValue.frame = CGRectMake(0, topIndent, self.view.bounds.size.width/2, indexLabelHeight);
    
    qpsValue.frame = CGRectMake(self.view.bounds.size.width/2, topIndent + indexLabelHeight + 25, self.view.bounds.size.width/2, indexLabelHeight);
    
    nepValue.frame = CGRectMake(self.view.bounds.size.width/2, topIndent, self.view.bounds.size.width/2, indexLabelHeight);
    
    graphView.frame = CGRectMake(0, 250, self.view.bounds.size.width, 400);
    
    infoButton.center = CGPointMake(self.view.bounds.size.width - 50, 30);
    
    helpView.frame = self.view.bounds;
}

- (void)reconnect;
{
    socket.delegate = nil;
    [socket close];
    socket = [[SRWebSocket alloc] initWithURLRequest: [NSURLRequest requestWithURL:[NSURL URLWithString:websocketUrl]]];
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
    webSocket = nil;
}

- (void)webSocket:(SRWebSocket *)webSocket didReceiveMessage:(id)message;
{
    NSString *messageString = ((NSString *)message);
    NSArray *dataPieces = [[messageString substringWithRange:NSMakeRange(1, messageString.length -2) ] componentsSeparatedByString:@","];
    if (dataPieces.count > 2){
        int i;
        for (i = 0; i < dataPieces.count/2 -1; i+=2){
            //we get such fromat data 2013-11-12T14:04:29Z
            NSString *dateString = [dataPieces[i] substringWithRange:NSMakeRange(8, ((NSString *)dataPieces[i]).length - 10)];
            dateString = [dateString stringByReplacingOccurrencesOfString:@"T" withString:@"_"];
            NSDate *date = [serverDateformatter dateFromString:dateString];
            NSString *nciVal = [dataPieces[i+1] substringFromIndex:6];
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
            NSString *endDate = [[serverDateformatter stringFromDate:
                                 [[NSDate date] dateByAddingTimeInterval: -timeAdjustment]]
                                 stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
            NSString *startDate = [[serverDateformatter stringFromDate:
                                  [[[NSDate date] dateByAddingTimeInterval: -timeAdjustment]
                                   dateByAddingTimeInterval: -60*60*24]]
                                   stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
             NSLog(websocketMoreDataRequest,  startDate, endDate);
            [webSocket send: [NSString stringWithFormat: websocketMoreDataRequest, startDate, endDate]];
        }
    }
}

- (void)webSocket:(SRWebSocket *)webSocket didCloseWithCode:(NSInteger)code reason:(NSString *)reason wasClean:(BOOL)wasClean;
{
    NSLog(@"WebSocket closed");
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
