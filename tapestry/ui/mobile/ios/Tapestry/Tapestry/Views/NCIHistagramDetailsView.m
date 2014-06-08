//
//  NCIHistagramDetailsView.m
//  Tapestry
//
//  Created by Ira on 6/8/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIHistagramDetailsView.h"
#import "NCIEndpoint.h"

@interface NCIHistagramDetailsView(){
    UIDynamicAnimator *animator;
    NSMutableDictionary *endpoints;
    NSDictionary *communityData;
}
@property(nonatomic)BOOL updating;
@end

@implementation NCIHistagramDetailsView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
      animator = [[UIDynamicAnimator alloc] initWithReferenceView:self];
    }
    return self;
}

- (void)showCommunity:(NSDictionary *)community{
    _updating = YES;
    communityData = community;
    [animator removeAllBehaviors];
//    UICollisionBehavior *collision = [[UICollisionBehavior alloc]
//                                      initWithItems:[endpoints allValues]];
//    collision.translatesReferenceBoundsIntoBoundary = YES;
//    [animator addBehavior:collision];
    endpoints = [[NSMutableDictionary alloc] init];
    for (UIView *view in self.subviews){
        [view removeFromSuperview];
    }
    float pointDimention = 16;
    for (NSString *ePoint in community[@"Endpoints"]){
        NCIEndpoint *ep = [[NCIEndpoint alloc] initWithFrame:
                           CGRectMake(arc4random() % (int)self.frame.size.width,
                                      pointDimention + arc4random() % (int)self.frame.size.height - 2*pointDimention,
                                      pointDimention, pointDimention)];
        ep.ip = ePoint;
        endpoints[ep.ip] = ep;
        ep.backgroundColor = [UIColor blackColor];
        [self addSubview:ep];
    }
    for (NSArray *interaction in community[@"Interactions"]){
        UIAttachmentBehavior *attachment = [[UIAttachmentBehavior alloc]
                                            initWithItem:endpoints[interaction[0]]
                                            attachedToItem:endpoints[interaction[1]]];
        [attachment setFrequency:0.0];
        [attachment setDamping:0.0];
        [attachment setLength:100];
        [animator addBehavior:attachment];
        [self performSelector:@selector(drawRect:) withObject:self afterDelay:1];
    }
    __weak typeof(self) weakSelf = self;
    dispatch_async(dispatch_get_global_queue(DISPATCH_QUEUE_PRIORITY_DEFAULT, 0), ^{
        while (weakSelf.updating) {
            [NSThread sleepForTimeInterval:0.05f];
            dispatch_async(dispatch_get_main_queue(), ^{
                [weakSelf setNeedsDisplay];
            });
        }
    });
}

- (void)drawRect:(CGRect)rect{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSetLineWidth(context, 0.2);
    CGContextBeginPath(context);
    CGContextSetStrokeColorWithColor(context, [UIColor blackColor].CGColor);
    for (NSArray *interaction in communityData[@"Interactions"]){
        UIView *p1 = endpoints[interaction[0]];
        UIView *p2 = endpoints[interaction[1]];
        CGContextMoveToPoint(context, p1.center.x, p1.center.y);
        CGContextAddLineToPoint(context, p2.center.x, p2.center.y);
    }
    CGContextStrokePath(context);
}

@end
