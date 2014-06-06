//
//  NCIConstants.m
//  Tapestry
//
//  Created by Infoblox Inc on 12/10/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIConstants.h"

@implementation NCIConstants

NSString *const demoUrl = @"demo";
NSString *const sampleUrl = @"nci.example.com:28080/clientsock.yaws";
float twoYearPeriod = 60*60*24*30*12*2;
float halfMonthPeriod = 60*60*12;

+ (NSString *)processTime:(NSString *) time{
    NSDateFormatter *serverDateformatter  = [[NSDateFormatter alloc] init];
    [serverDateformatter setDateFormat:@"yyyy-MM-dd HH:mm:ss"];
    time = [[time stringByReplacingOccurrencesOfString:@"T" withString:@" "] stringByReplacingOccurrencesOfString:@"Z" withString:@""];
    [serverDateformatter setTimeZone:[NSTimeZone timeZoneForSecondsFromGMT:0]];
    NSDate *date = [serverDateformatter dateFromString:time];
    [serverDateformatter setTimeZone:[NSTimeZone  localTimeZone]];
    return [serverDateformatter stringFromDate:date];
}

@end
