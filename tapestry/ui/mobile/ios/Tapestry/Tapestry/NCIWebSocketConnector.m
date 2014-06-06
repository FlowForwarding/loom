//
//  NCIWebSocketConnector.m
//  Tapestry
//
//  Created by Infoblox Inc on 11/21/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
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
static NSString* websocketCollectorsDetailsRequest =
@"{\"action\":\"collectors\",\"Time\": \"%@Z\"}";
static NSString* websocketNCIDetailsRequest =
@"{\"action\":\"NCIDetails\",\"Time\": \"%@Z\"}";

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
    self.progressLabel.hidden = NO;
    //    [self.chartView resetChart];
    //    [self.chartView drawChart];
    
    NSString *endDateString = [self formatDataForServer: endDate];
    NSString *startDateString = [self formatDataForServer:startDate];
    [socket send: [NSString stringWithFormat: websocketMoreDataRequest, startDateString, endDateString]];
}

- (void)requestCollecotrsDetails:(NSString *) date{
    if (demoMode){
        NSMutableArray *collecotrs = [[NSMutableArray alloc] init];
        for (int i = 1; i<8; i++){
            [collecotrs addObject:@{
                                    @"name": [NSString stringWithFormat:@"collector %d", i],
                                    @"collector_type": @"switch",
                                    @"ip": @"",
                                    @"datapath_id": @"",
                                    @"qps": [NSString stringWithFormat:@"%d", arc4random() % 5 + 1]}];
        }
        NSData *responseData = [NSJSONSerialization
                                dataWithJSONObject:@{
                                                     @"action": @"collectors",
                                                     @"Time": date,
                                                     @"Collectors" : collecotrs
                                                     }
                                options:0 error:nil];
        [self webSocket:nil didReceiveMessage: [[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding]];
    } else {
       [socket send: [NSString stringWithFormat: websocketCollectorsDetailsRequest, date]];
    }
}

- (void)requestNCIDetails:(NSString *) date{
    if (demoMode){
        NSMutableArray *communities = [[NSMutableArray alloc] init];
        for (int i = 0; i< (arc4random()%3 + 3); i ++){
            NSMutableArray *endpoints = [[NSMutableArray alloc] init];
            for (int i=0; i< (arc4random() % 5 + 2); i ++){
                [endpoints addObject:@"123"];
            }
            NSMutableArray *interactions = [[NSMutableArray alloc] init];
            [communities addObject:@{@"Endpoints": endpoints, @"Interactions": interactions}];
        }
        NSData *responseData = [NSJSONSerialization
                                dataWithJSONObject:@{
                                                     @"action": @"NCIDetails",
                                                     @"NCI": @3,
                                                     @"Time": date,
                                                     @"Communities": communities
                                                     }
                                options:0 error:nil];
        [self webSocket:nil didReceiveMessage: [[NSString alloc] initWithData:responseData encoding:NSUTF8StringEncoding]];
    } else {
        [socket send: [NSString stringWithFormat: websocketNCIDetailsRequest, date]];
    }
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
    self.progressLabel.hidden = NO;
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
    float demoDatePeriod = halfMonthPeriod;
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        int trendMiddle = 6;
        int trendStepCounter = 0;
        
        NSDictionary *response = @{@"start_time": [self formatDataForServer:[[NSDate date] dateByAddingTimeInterval: -demoDatePeriod]],
                                   @"current_time": [self formatDataForServer:[NSDate date]]};
        [self sendDemoData:response];
        
        NSString *moreResponse = @"{";
        float numOfPoints = 800;
        float step = demoDatePeriod/(numOfPoints - 1);
        int ind;
        for (ind = 0; ind < numOfPoints; ind ++){
            if (trendStepCounter > 140){
                trendStepCounter = 0;
                trendMiddle += 1;
            }
            trendStepCounter += 1;
            int complexity = trendMiddle + arc4random() % 3;
            moreResponse = [NSString stringWithFormat:@"%@\"Time\":\"%@\",\"NCI\":%d,",
                            moreResponse,
                            [self formatDataForServer:[[NSDate date] dateByAddingTimeInterval: (-demoDatePeriod + step*ind)]],
                            complexity];
        }
        moreResponse = [moreResponse stringByReplacingCharactersInRange:NSMakeRange(moreResponse.length - 1, 1) withString:@"}"];
        [self sendDemoString:moreResponse];
        
        while (demoMode){
            if (trendStepCounter > 140){
                trendStepCounter = 0;
                trendMiddle += 1;
            }
            trendStepCounter += 1;
            int complexity = trendMiddle + arc4random() % 3;
            NSDictionary *response = @{@"NCI":[NSString stringWithFormat: @"%d", complexity],
                                       @"Time": [self formatDataForServer: [NSDate date]],
                                       @"action": @"NCI"};
            [self sendDemoData:response];
            int endpoints = 310 + arc4random() % 5;
            NSDictionary *response2 = @{@"NEP": [NSString stringWithFormat: @"%d", endpoints],
                                        @"Time": [self formatDataForServer: [NSDate date]],
                                        @"action": @"NEP"};
            [self sendDemoData:response2];
            int quieries  = 230 + arc4random() % 5;
            NSDictionary *response3 = @{@"QPS":[NSString stringWithFormat: @"%d", quieries],
                                        @"Time": [self formatDataForServer: [NSDate date]],
                                        @"action": @"QPS"};
            [self sendDemoData:response3];
            int collectors = arc4random() % 4 + 1;
            NSDictionary *response4 = @{@"COLLECTORS":[NSString stringWithFormat: @"%d", collectors],
                                        @"Time": [self formatDataForServer: [NSDate date]],
                                        @"action": @"Collectors"};
            [self sendDemoData:response4];
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
    self.progressLabel.hidden = YES;
    NSLog(@"Websocket Failed With Error %@", error);
    [self.noConnection setHidden:NO];
    webSocket = nil;
}

- (void)webSocket:(SRWebSocket *)webSocket didReceiveMessage:(id)message;
{
    NSString *messageString = ((NSString *)message);
    NSArray *dataPieces = [[messageString substringWithRange:NSMakeRange(1, messageString.length -2) ] componentsSeparatedByString:@","];
    NSDictionary *dataPoint = [NSJSONSerialization
                               JSONObjectWithData:[message dataUsingEncoding:NSUTF8StringEncoding]
                               options:NSJSONReadingMutableContainers error:NULL];
    if ([dataPoint[@"action"] isEqualToString:@"NCI"]){
        NSString *nci = dataPoint[@"NCI"];
        self.progressLabel.hidden = YES;
        [self.nciValue setIndValue:nci withDate:dataPoint[@"Time"]];
        if (_currentDatePeriod == twoYearPeriod){
            if (self.chartView.chartData.count > 0)
                [self.chartView.chartData removeLastObject];
            [self.chartView addPoint:[[self dateFromServerString: dataPoint[@"Time"]] timeIntervalSince1970] val:@[@([nci integerValue])]];
            while ([[NSDate date] timeIntervalSince1970] - [self.chartView.chartData[0][0] doubleValue] > _currentDatePeriod){
                [self.chartView.chartData removeObjectAtIndex:0];
            }
            [self.chartView addPoint:[[NSDate date] timeIntervalSince1970] val:@[[NSNull null]]];
            [self.chartView drawChart];
        }
    } else if([dataPoint[@"action"] isEqualToString:@"NEP"]){
        [self.nepValue setIndValue: dataPoint[@"NEP"] withDate:dataPoint[@"Time"]];
    } else if([dataPoint[@"action"] isEqualToString:@"QPS"]) {
        [self.qpsValue setIndValue:dataPoint[@"QPS"] withDate:dataPoint[@"Time"]];
    } else if([dataPoint[@"action"] isEqualToString:@"Collectors"]){
        [self.collectorsValue setIndValue:dataPoint[@"COLLECTORS"] withDate:dataPoint[@"Time"]];
    } else if([dataPoint[@"action"] isEqualToString:@"collectors"]){
        [self.collectorsDetailsView loadData:dataPoint];
    } else if([dataPoint[@"action"] isEqualToString:@"NCIDetails"]){
        [self.detailsView loadData:dataPoint];
    } else if(dataPoint[@"start_time"]){
         NSString *start_time = dataPoint[@"start_time"];
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
    } else {
        self.progressLabel.hidden = YES;
        [self.chartView resetChart];
        int i;
        for (i = 0; i < dataPieces.count; i+=2){
            //we get such fromat data 2013-11-12T14:04:29Z
            NSString *dateString = [dataPieces[i] substringWithRange:NSMakeRange(8, ((NSString *)dataPieces[i]).length - 10)];
            NSDate *date = [self dateFromServerString: dateString];
            NSString *nciVal = [dataPieces[i+1] substringFromIndex:6];
            [self.chartView addPoint:[date timeIntervalSince1970] val:@[@([nciVal integerValue])]];
        }
        [self.chartView addPoint:[[NSDate date]  timeIntervalSince1970] val:@[[NSNull null]]];
        long period = [[NSDate date] timeIntervalSince1970] - [[self.chartView.chartData firstObject][0] doubleValue];
        self.chartView.maxRangeVal = [[NSDate date] timeIntervalSince1970];
        self.chartView.minRangeVal = self.chartView.maxRangeVal - period /10.0;
        [self.chartView drawChart];
        
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
