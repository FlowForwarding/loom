//
//  NCIBottomGraphView.h
//  Tapestry
//
//  Created by Ira on 11/19/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGraphView.h"

@interface NCIBottomGraphView : NCIGraphView

- (void)redrawRanges;

- (void)startMoveWithPoint:(CGPoint) point1 andPoint:(CGPoint) point2;
- (void)moveReverseRangesWithPoint:(CGPoint) point1 andPoint:(CGPoint) point2;

@end
