//
//  NCIFlowsView.m
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIFlowsView.h"
#import "NCIEndpoint.h"

@interface  NCIFlowsView(){
    NSMutableDictionary *endpoints;
    NSMutableArray *groupColors;
    UIDynamicAnimator *animator;
    NSArray *curData;
}
@end

@implementation NCIFlowsView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        UILabel *label = [[UILabel alloc] initWithFrame:self.bounds];
        label.textAlignment = NSTextAlignmentCenter;
       // label.text = @"Connetions graph here";
        [self addSubview:label];
        animator = [[UIDynamicAnimator alloc] initWithReferenceView:self];
        self.backgroundColor = [UIColor whiteColor];
        groupColors = [[NSMutableArray alloc] initWithArray:@[[UIColor blueColor], [UIColor greenColor], [UIColor purpleColor]]];
    }
    return self;
}

- (void)loadData:(NSArray *) communities{
    _updating = true;
    _communitiesData = communities;
    
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

- (void)showFlows{
    curData = _communitiesData;
    for (UIView *view in self.subviews){
        [view removeFromSuperview];
    }
    endpoints = [[NSMutableDictionary alloc] init];
    float pointDimention = 12;
    for (int i=0; i< _communitiesData.count; i++){
        NSDictionary* community = _communitiesData[i];
        for (NSString *ePoint in community[@"Endpoints"]){
            NCIEndpoint *ep = [[NCIEndpoint alloc] initWithFrame:
                               CGRectMake(arc4random() % (int)self.frame.size.width,
                                          pointDimention + arc4random() % (int)self.frame.size.height - 2*pointDimention,
                                          pointDimention, pointDimention)];
            ep.group = i;
            ep.ip = ePoint;
            ep.backgroundColor = [self getColor:0];
            [self addSubview:ep];
            endpoints[ePoint] = ep;
        }
    }
}

- (void)colorifyEndpoints{
    for (NCIEndpoint *point in [endpoints allValues]){
        point.backgroundColor = [self getColor:point.group];
    }
}

- (void)showActivities{
    curData = _communityGraphData;
    for (UIView *view in self.subviews){
        [view removeFromSuperview];
    }
    endpoints = [[NSMutableDictionary alloc] init];
    float pointDimention = 12;
    for (int i=0; i< _communityGraphData.count; i++){
        NSDictionary* community = _communityGraphData[i];
        for (NSString *ePoint in community[@"Endpoints"]){
            NCIEndpoint *ep = [[NCIEndpoint alloc] initWithFrame:
                               CGRectMake(arc4random() % (int)self.frame.size.width,
                                          pointDimention + arc4random() % (int)self.frame.size.height - 2*pointDimention,
                                          pointDimention, pointDimention)];
            ep.group = i;
            ep.ip = ePoint;
            ep.backgroundColor = [self getColor:0];
            [self addSubview:ep];
            endpoints[ePoint] = ep;
        }
    }
}

- (void)showPrettyActivities{
    UIDynamicItemBehavior *behavior = [[UIDynamicItemBehavior alloc]  initWithItems:[endpoints allValues]];
    behavior.friction = 1;
    behavior.resistance = 1;
    [animator addBehavior:behavior];
    UICollisionBehavior *collision = [[UICollisionBehavior alloc]
                                      initWithItems:[endpoints allValues]];
    collision.translatesReferenceBoundsIntoBoundary = YES;
    [animator addBehavior:collision];
    for (int i=0; i< _communityGraphData.count; i++){
        NSDictionary* community = _communityGraphData[i];
        for (NSArray *interaction in community[@"Interactions"]){
            UIAttachmentBehavior *attachment = [[UIAttachmentBehavior alloc]
                                                initWithItem:endpoints[interaction[0]]
                                                attachedToItem:endpoints[interaction[1]]];
            [attachment setFrequency:10.0];
            //[attachment setDamping:5.0];
            [attachment setLength:100];
            [animator addBehavior:attachment];
        }
    }

}

- (void)showInternalActivities{
    
}

- (void)showInternal{
    for (NCIEndpoint *point in [endpoints allValues]){
        if (point.ip) {
             point.backgroundColor = [UIColor lightGrayColor];
        } else {
             point.backgroundColor = [self getColor:point.group];
        }
    }
}

- (void)applyForces{
    UIDynamicItemBehavior *behavior = [[UIDynamicItemBehavior alloc]  initWithItems:[endpoints allValues]];
    behavior.friction = 1;
    behavior.resistance = 1;
    [animator addBehavior:behavior];
    UICollisionBehavior *collision = [[UICollisionBehavior alloc]
                                      initWithItems:[endpoints allValues]];
    collision.translatesReferenceBoundsIntoBoundary = YES;
    [animator addBehavior:collision];
    for (int i=0; i< _communitiesData.count; i++){
        NSDictionary* community = _communitiesData[i];
        for (NSArray *interaction in community[@"Interactions"]){
            UIAttachmentBehavior *attachment = [[UIAttachmentBehavior alloc]
                                                initWithItem:endpoints[interaction[0]]
                                                attachedToItem:endpoints[interaction[1]]];
            [attachment setFrequency:10.0];
            //[attachment setDamping:5.0];
            [attachment setLength:100];
            [animator addBehavior:attachment];
        }
    }
}

- (void)drawRect:(CGRect)rect{
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSetLineWidth(context, 0.2);
    CGContextBeginPath(context);
    CGContextSetStrokeColorWithColor(context, [UIColor blackColor].CGColor);
    for (int i=0; i< curData.count; i++){
        NSDictionary* community = curData[i];
        for (NSArray *interaction in community[@"Interactions"]){
            UIView *p1 = endpoints[interaction[0]];
            UIView *p2 = endpoints[interaction[1]];
            CGContextMoveToPoint(context, p1.center.x, p1.center.y);
            CGContextAddLineToPoint(context, p2.center.x, p2.center.y);
        }
    }
    CGContextStrokePath(context);
}

- (UIColor *)getColor:(int) i{
    if (groupColors.count > i){
        return groupColors[i];
    } else {
        UIColor *newColor = [UIColor colorWithRed:(arc4random() % 255)/255.0f
                                            green:(arc4random() % 255)/255.0f
                                             blue:(arc4random() % 255)/255.0f alpha:1.0];
        [groupColors addObject:newColor];
        return newColor;
    }
}

//- (void)hideActions{
//    [endpoints removeAllObjects];
//   
//}


@end
