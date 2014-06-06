//
//  NCIEndpoint.m
//  Tapestry
//
//  Created by Ira on 5/23/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIEndpoint.h"

@interface NCIEndpoint()

@end

@implementation NCIEndpoint

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        self.layer.cornerRadius = frame.size.width/2;
    }
    return self;
}


@end
