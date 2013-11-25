//
//  NCIWebSocketConnector.m
//  Tapestry
//
//  Created by Ira on 11/21/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIWebSocketConnector.h"
#import "SRWebSocket.h"

//TODO seporate UI and request/responce logic
float halfMonthPeriod = 60*60*24*15;
float yearPeriod = 60*60*24*30*12;
float tenYearsPeriod = 60*60*24*30*12;

@interface NCIWebSocketConnector()<SRWebSocketDelegate>{
    SRWebSocket *socket;
    //int timeAdjustment;
}
@property(nonatomic, strong)NSDateFormatter *serverDateformatter;

@end

static NSString* websocketStartRequest = @"START_DATA";
static NSString* websocketMoreDataRequest =
@"{\"request\":\"more_data\",\"start\": \"%@Z\",\"end\": \"%@Z\",\"max_items\": \"300\"}";

@implementation NCIWebSocketConnector

+ (NCIWebSocketConnector *)interlocutor{
    static NCIWebSocketConnector *interlocutor;
    
    @synchronized(self)
    {
        if (!interlocutor)
            interlocutor = [[NCIWebSocketConnector alloc] init];
        interlocutor.serverDateformatter = [[NSDateFormatter alloc] init];
        [interlocutor.serverDateformatter setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
        [interlocutor.serverDateformatter setDateFormat:@"yyyy-MM-dd_HH:mm:ss"];
        return interlocutor;
    }
}

- (void)requestLastDataForPeiodInSeconds:(int) period{
    NSDate *endDate = [NSDate date];//[[NSDate date] dateByAddingTimeInterval: -timeAdjustment];
    NSDate *startDate = [[NSDate date]
                         dateByAddingTimeInterval: - period];
    [self.chartView resetChart];
    [self.chartView drawChart];
    
    NSString *endDateString = [[self.serverDateformatter stringFromDate: endDate]
                               stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
    NSString *startDateString = [[self.serverDateformatter stringFromDate:startDate]
                                 stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
    [socket send: [NSString stringWithFormat: websocketMoreDataRequest, startDateString, endDateString]];
}

- (void)resetData{
    [self.nciValue resetData];
    [self.qpsValue resetData];
    [self.nepValue resetData];
    [self.noConnectionLabel setHidden:YES];
    [self.chartView resetChart];
    [[NCIWebSocketConnector interlocutor] reconnect];
}


- (void)reconnect
{
    socket.delegate = nil;
    [socket close];
    socket = [[SRWebSocket alloc] initWithURLRequest: [NSURLRequest requestWithURL: [NSURL URLWithString: [self.editServerView getServerUrl]]]];
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
    [self.noConnectionLabel setHidden:NO];
    webSocket = nil;
}

- (void)webSocket:(SRWebSocket *)webSocket didReceiveMessage:(id)message;
{
    NSString *messageString = ((NSString *)message);
    NSArray *dataPieces = [[messageString substringWithRange:NSMakeRange(1, messageString.length -2) ] componentsSeparatedByString:@","];
    if (dataPieces.count > 2){
        [self.chartView resetChart];
        int i;
        for (i = 0; i < dataPieces.count/2; i+=2){
            //we get such fromat data 2013-11-12T14:04:29Z
            NSString *dateString = [dataPieces[i*2] substringWithRange:NSMakeRange(8, ((NSString *)dataPieces[i]).length - 10)];
            NSDate *date = [self dateFromServerString: dateString];
            NSString *nciVal = [dataPieces[2*i+1] substringFromIndex:6];
            [self.chartView addPoint:date val:nciVal];
        }
        _currentDatePeriod = [[NSDate date] timeIntervalSince1970] - [((NSDate *)[self.chartView.chartData firstObject][0]) timeIntervalSince1970];
        self.chartView.minRangeDate = [[NSDate date] dateByAddingTimeInterval: - _currentDatePeriod /6.0];
        self.chartView.maxRangeDate = [NSDate date];
        [self.chartView setMaxArgument: [NSDate date]];
        [self.chartView drawChart];
        
    } else {
        NSDictionary *dataPoint = [NSJSONSerialization
                                   JSONObjectWithData:[message dataUsingEncoding:NSUTF8StringEncoding]
                                   options:NSJSONReadingMutableContainers error:NULL];
        
        NSString *nci = dataPoint[@"NCI"];
        NSString *nep = dataPoint[@"NEP"];
        NSString *qps = dataPoint[@"QPS"];
        if (nci){
            [self.nciValue setIndValue:nci withDate:dataPoint[@"Time"]];
            [self.chartView addPoint:[self dateFromServerString: dataPoint[@"Time"]] val:nci];
            self.chartView.maxRangeDate = [NSDate date];
            [self.chartView drawChart];
        } else if (nep) {
            [self.nepValue setIndValue:nep  withDate:dataPoint[@"Time"]];
        } else if (qps) {
            [self.qpsValue setIndValue:qps withDate:dataPoint[@"Time"]];
        }
        //{"start_time":"2013-11-13T16:42:55Z","current_time":"2013-11-13T21:23:47Z"}
        NSString *start_time = dataPoint[@"start_time"];
        if (start_time){
            _startDate = [self dateFromServerString:start_time];
            NSString *current_time = dataPoint[@"current_time"];
            NSDate *date = [self dateFromServerString:current_time];
            //timeAdjustment = [date timeIntervalSinceNow];
            
            [self requestLastDataForPeiodInSeconds:halfMonthPeriod];
        }
    }
}

- (NSDate *)dateFromServerString:(NSString *)str{
    str = [str stringByReplacingOccurrencesOfString:@"T" withString:@"_"];
    str = [str stringByReplacingOccurrencesOfString:@"Z" withString:@""];
    return [self.serverDateformatter dateFromString:str];
}

- (void)webSocket:(SRWebSocket *)webSocket didCloseWithCode:(NSInteger)code reason:(NSString *)reason wasClean:(BOOL)wasClean;
{
    NSLog(@"WebSocket closed");
    [self.noConnectionLabel setHidden:NO];
    socket = nil;
}


@end
