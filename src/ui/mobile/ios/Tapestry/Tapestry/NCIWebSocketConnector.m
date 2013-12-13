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
static int bookmarkMaxRowsCount = 20;

@interface NCIWebSocketConnector()<SRWebSocketDelegate>{
    SRWebSocket *socket;
    bool demoMode;
    //int timeAdjustment;
}
@property(nonatomic, strong)NSDateFormatter *serverDateformatter;
@property(nonatomic, strong)NSString *currentUrl;

@end

static NSString* defaultWebsocketUrl = @"epamove.herokuapp.com";
static NSString* websocketStartRequest = @"START_DATA";
static NSString* websocketMoreDataRequest =
@"{\"request\":\"more_data\",\"start\": \"%@Z\",\"end\": \"%@Z\",\"max_items\": \"800\"}";

@implementation NCIWebSocketConnector

+ (NCIWebSocketConnector *)interlocutor{
    static NCIWebSocketConnector *interlocutor;
    
    @synchronized(self)
    {
        if (!interlocutor){
            interlocutor = [[NCIWebSocketConnector alloc] init];
            interlocutor.tapestryURLs = [[NSUserDefaults standardUserDefaults] objectForKey:@"tapestryUrls"];
            if (!interlocutor.tapestryURLs){
                interlocutor.tapestryURLs = [[NSMutableArray alloc] init];
            }
            if (interlocutor.tapestryURLs.count == 0){
                interlocutor.currentUrl = demoUrl;
            } else {
                interlocutor.currentUrl = interlocutor.tapestryURLs[0];
            }
            interlocutor.serverDateformatter = [[NSDateFormatter alloc] init];
            [interlocutor.serverDateformatter setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
            [interlocutor.serverDateformatter setDateFormat:@"yyyy-MM-dd_HH:mm:ss"];
        }
        return interlocutor;
    }
}

- (void)newTapestryUrl:(NSString *) newUrl{
    int ind;
    _currentUrl = newUrl;
    NSString *url;
    for (ind = 0; ind < self.tapestryURLs.count; ind++){
        url = self.tapestryURLs[ind];
        if ([url isEqualToString:newUrl]){
            [self.tapestryURLs removeObjectAtIndex:ind];
            break;
        }
    }
    [self.tapestryURLs insertObject:newUrl atIndex:0];
    if (self.tapestryURLs.count >  bookmarkMaxRowsCount){
        [self.tapestryURLs removeLastObject];
    }
    [[NSUserDefaults standardUserDefaults] setObject:self.tapestryURLs forKey:@"tapestryUrls"];
}


- (NSString *)getTapestryUrl{
    return _currentUrl;
}

- (void)removeURLAtIndex:(long)index{
    [_tapestryURLs removeObjectAtIndex:index];
    [[NSUserDefaults standardUserDefaults] setObject:self.tapestryURLs forKey:@"tapestryUrls"];
}

- (void)requestLastDataForPeiodInSeconds:(float) period{
    NSDate *endDate = [NSDate date];//[[NSDate date] dateByAddingTimeInterval: -timeAdjustment];
    NSDate *startDate = [[NSDate date]
                         dateByAddingTimeInterval: - period];
    //    [self.chartView resetChart];
    //    [self.chartView drawChart];
    
    NSString *endDateString = [self formatDataForServer: endDate];
    NSString *startDateString = [self formatDataForServer:startDate];
    [socket send: [NSString stringWithFormat: websocketMoreDataRequest, startDateString, endDateString]];
}

- (void)resetData{
    [self.periodSwitcherPanel resetButtons];
    [self.nciValue resetData];
    [self.qpsValue resetData];
    [self.nepValue resetData];
    [self.noConnection setHidden:YES];
    [self.chartView resetChart];
    [self.chartView drawChart];
    [[NCIWebSocketConnector interlocutor] reconnect];
}


- (void)reconnect
{
    socket.delegate = nil;
    [socket close];
    if([[self getTapestryUrl] isEqualToString:demoUrl]){
        demoMode = YES;
        [self generateDemoData];
    } else {
        demoMode = NO;
        socket = [[SRWebSocket alloc] initWithURLRequest: [NSURLRequest requestWithURL:
                                                           [NSURL URLWithString: [@"ws://" stringByAppendingString: [self getTapestryUrl]]]]];
        socket.delegate = self;
        [socket open];
    }
}

- (void)generateDemoData{
    float demoDatePeriod = twoYearPeriod/4.0;
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        int trendMiddle = 6;
        int trendStepCounter = 0;
        
        NSDictionary *response = @{@"start_time": [self formatDataForServer:[[NSDate date] dateByAddingTimeInterval: -demoDatePeriod]],
                                   @"current_time": [self formatDataForServer:[NSDate date]]};
        [self sendDemoData:response];
        
        NSString *moreResponse = @"{";
        float numOfPoints = 800;
        float step = demoDatePeriod/(800 - 1);
        int ind;
        for (ind = 0; ind < numOfPoints; ind ++){
            if (trendStepCounter > 5){
                trendStepCounter = 0;
                trendMiddle += 1;
            }
            trendStepCounter += 1;
            int complexity = trendMiddle + arc4random() % 5;
            moreResponse = [NSString stringWithFormat:@"%@\"Time\":\"%@\",\"NCI\":%d,",
                            moreResponse,
                            [self formatDataForServer:[[NSDate date] dateByAddingTimeInterval: (-demoDatePeriod + step*ind)]],
                            complexity];
        }
        moreResponse = [moreResponse stringByReplacingCharactersInRange:NSMakeRange(moreResponse.length - 1, 1) withString:@"}"];
        [self sendDemoString:moreResponse];
        
        while (demoMode){
            if (trendStepCounter > 5){
                trendStepCounter = 0;
                trendMiddle += 1;
            }
            trendStepCounter += 1;
            int complexity = trendMiddle + arc4random() % 5;
            NSDictionary *response = @{@"NCI":[NSString stringWithFormat: @"%d", complexity]
                                       , @"Time": [self formatDataForServer: [NSDate date]]};
            [self sendDemoData:response];
            int endpoints = 310 + arc4random() % 5;
            NSDictionary *response2 = @{@"NEP": [NSString stringWithFormat: @"%d", endpoints],
                                        @"Time": [self formatDataForServer: [NSDate date]]};
            [self sendDemoData:response2];
            int quieries  = 230 + arc4random() % 5;
            NSDictionary *response3 = @{@"QPS":[NSString stringWithFormat: @"%d", quieries],
                                        @"Time": [self formatDataForServer: [NSDate date]]};
            [self sendDemoData:response3];
            [NSThread sleepForTimeInterval:3.0f];
        }
    });
}

- (void)sendDemoString:(NSString* )dataString{
    dispatch_async(dispatch_get_main_queue(), ^{
        [self webSocket:nil didReceiveMessage: dataString];
    });
}

- (void)sendDemoData:(NSDictionary *)dataDict{
    NSData *responseData = [NSJSONSerialization dataWithJSONObject:dataDict options:0 error:nil];
    dispatch_async(dispatch_get_main_queue(), ^{
        [self webSocket:nil didReceiveMessage: [[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding]];
    });
}

- (NSString *)formatDataForServer:(NSDate *)date{
    return [[self.serverDateformatter stringFromDate: date ]
            stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
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
    [self.noConnection setHidden:NO];
    webSocket = nil;
}

- (void)webSocket:(SRWebSocket *)webSocket didReceiveMessage:(id)message;
{
    NSString *messageString = ((NSString *)message);
    NSArray *dataPieces = [[messageString substringWithRange:NSMakeRange(1, messageString.length -2) ] componentsSeparatedByString:@","];
    if (dataPieces.count > 2){
        [self.chartView resetChart];
        int i;
        for (i = 0; i < dataPieces.count; i+=2){
            //we get such fromat data 2013-11-12T14:04:29Z
            NSString *dateString = [dataPieces[i] substringWithRange:NSMakeRange(8, ((NSString *)dataPieces[i]).length - 10)];
            NSDate *date = [self dateFromServerString: dateString];
            NSString *nciVal = [dataPieces[i+1] substringFromIndex:6];
            [self.chartView addPoint:date val:nciVal];
        }
        if (!self.chartView.minRangeDate){
            long period = [[NSDate date] timeIntervalSince1970] - [((NSDate *)[self.chartView.chartData firstObject][0]) timeIntervalSince1970];
            self.chartView.minRangeDate = [[NSDate date] dateByAddingTimeInterval: - period /10.0];
            self.chartView.maxRangeDate = [NSDate date];
        }
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
            if (_currentDatePeriod == twoYearPeriod){
                [self.chartView addPoint:[self dateFromServerString: dataPoint[@"Time"]] val:nci];
                while ([[NSDate date] timeIntervalSince1970] - [self.chartView.chartData[0][0] timeIntervalSince1970] > _currentDatePeriod){
                    [self.chartView removeFirstPoint];
                }
                [self.chartView setMaxArgument:[NSDate date]];
                if (self.chartView.chartData.count == 1){
                    [self.chartView setMinArgument:[NSDate date]];
                } else {
                    [self.chartView drawChart];
                }
            }
        } else if (nep) {
            [self.nepValue setIndValue:nep  withDate:dataPoint[@"Time"]];
        } else if (qps) {
            [self.qpsValue setIndValue:qps withDate:dataPoint[@"Time"]];
        }
        //{"start_time":"2013-11-13T16:42:55Z","current_time":"2013-11-13T21:23:47Z"}
        NSString *start_time = dataPoint[@"start_time"];
        if (start_time){
            self.startDate = [self dateFromServerString:start_time];
//            NSString *current_time = dataPoint[@"current_time"];
//            NSDate *date = [self dateFromServerString:current_time];
//            timeAdjustment = [date timeIntervalSinceNow];
            double askPeriod = [[NSDate date] timeIntervalSince1970] - [self.startDate timeIntervalSince1970];
            if (askPeriod > twoYearPeriod){
                askPeriod = twoYearPeriod;
            }
            _currentDatePeriod = twoYearPeriod;
            [self requestLastDataForPeiodInSeconds:askPeriod];
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
    [self.noConnection setHidden:NO];
    socket = nil;
}


@end
