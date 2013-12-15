//
//  NCIBottomGraphView.h
//  Tapestry
//
//  Created by Infoblox Inc on 11/19/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import "NCIGraphView.h"

@interface NCIBottomGraphView : NCIGraphView

- (void)redrawRanges;

- (void)startMoveWithPoint:(CGPoint) point1 andPoint:(CGPoint) point2;
- (void)moveReverseRangesWithPoint:(CGPoint) point1 andPoint:(CGPoint) point2;

@end
