//
//  NCIConstants.h
//  Tapestry
//
//  Created by Infoblox Inc on 12/10/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import <Foundation/Foundation.h>

@interface NCIConstants : NSObject

extern NSString *const demoUrl;
extern NSString *const sampleUrl;
extern float twoYearPeriod;
extern float halfMonthPeriod;

+ (NSString *)processTime:(NSString *) time;

@end
