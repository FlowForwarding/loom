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

@interface NCIGraphController() <SRWebSocketDelegate>{
    SRWebSocket *socket;
    NCIIndexValueView *nciValue;
    NCIIndexValueView *nepValue;
    NCIIndexValueView *qpsValue;
}
@end

static NSString* websocketUrl = @"secret";
static NSString* websocketStartRequest = @"START_DATA";

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
    int topIndent = 100;
    int indexLabelHeight = 60;
    self.title = NSLocalizedString(@"Tapestry: A Network Complexity Analyzer", nil);
    
    nciValue = [[NCIIndexValueView alloc] initWithFrame:CGRectMake(0, topIndent, self.view.bounds.size.width/2, indexLabelHeight)
                                                indName:@"NCI" indSize:14];
    [self.view addSubview:nciValue];
    nepValue = [[NCIIndexValueView alloc] initWithFrame:CGRectMake(self.view.bounds.size.width/2,
                                                         topIndent, self.view.bounds.size.width/2, indexLabelHeight)
                                                indName:@"NEP" indSize:14];
    [self.view addSubview:nepValue];
    qpsValue = [[NCIIndexValueView alloc] initWithFrame:CGRectMake(self.view.bounds.size.width/2,
                                                         topIndent + indexLabelHeight, self.view.bounds.size.width/2, indexLabelHeight)
                                                indName:@"QPS" indSize:14];
    [self.view addSubview:qpsValue];
    
    [self reconnect];
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
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
    NSDictionary *dataPoint = [NSJSONSerialization
                               JSONObjectWithData:[message dataUsingEncoding:NSUTF8StringEncoding]
                               options:NSJSONReadingMutableContainers error:NULL];
    
   // NSLog(@"dataPoint %@", dataPoint);
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
}

- (void)webSocket:(SRWebSocket *)webSocket didCloseWithCode:(NSInteger)code reason:(NSString *)reason wasClean:(BOOL)wasClean;
{
    NSLog(@"WebSocket closed");
    socket = nil;
}

@end
