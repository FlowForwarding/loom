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
static float oneYearPeriod = 60*60*24*30*12;
static int bookmarkMaxRowsCount = 20;

@interface NCIWebSocketConnector()<SRWebSocketDelegate>{
    SRWebSocket *socket;
    //int timeAdjustment;
}
@property(nonatomic, strong)NSDateFormatter *serverDateformatter;

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
                interlocutor.tapestryURLs = [[NSMutableArray alloc] initWithArray:@[defaultWebsocketUrl]];
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
    return self.tapestryURLs[0];
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
        
        NSString *endDateString = [[self.serverDateformatter stringFromDate: endDate]
                                   stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
        NSString *startDateString = [[self.serverDateformatter stringFromDate:startDate]
                                     stringByReplacingOccurrencesOfString:@"_" withString:@"T"];
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
    socket = [[SRWebSocket alloc] initWithURLRequest: [NSURLRequest requestWithURL:
                                                       [NSURL URLWithString: [@"ws://" stringByAppendingString: [self getTapestryUrl]]]]];
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
        long period = [[NSDate date] timeIntervalSince1970] - [((NSDate *)[self.chartView.chartData firstObject][0]) timeIntervalSince1970];
        self.chartView.minRangeDate = [[NSDate date] dateByAddingTimeInterval: - period /10.0];
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
            if (_currentDatePeriod == oneYearPeriod){
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
            if (askPeriod > oneYearPeriod){
                askPeriod = oneYearPeriod;
            }
            _currentDatePeriod = oneYearPeriod;
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
