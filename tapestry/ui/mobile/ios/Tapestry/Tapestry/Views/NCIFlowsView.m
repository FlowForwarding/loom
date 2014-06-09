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
    UILabel *infoLabel;
    BOOL isFlows;
    BOOL isColorified;
    BOOL isPretty;
    BOOL isInternal;
    BOOL isToMuch;
}
@end

@implementation NCIFlowsView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        infoLabel = [[UILabel alloc] initWithFrame:self.bounds];
        infoLabel.textAlignment = NSTextAlignmentCenter;
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
    isFlows = YES;
    isColorified = NO;
    isPretty = NO;
    isInternal = NO;
    curData = _communitiesData;
    [self drawPoints];
}

- (void)colorifyEndpoints{
    if (!isFlows || isPretty)
        [self showFlows];
    if (isToMuch)
        return;
    isColorified = YES;
    isPretty = NO;
    isInternal = NO;
    [self colorify];
}

- (void)showPrettyFlows{
    if (!isFlows){
        [self showFlows];
    }
    if (isToMuch)
        return;
    if (!isColorified || isInternal){
        [self colorify];
        isColorified = YES;
    }
    isPretty = YES;
    isInternal = NO;
    [self applyForces];
}

- (void)showInternalFlows{
    if (!isPretty)
        [self showPrettyFlows];
    if (isToMuch)
        return;
    isInternal = YES;
    [self showInternal];
}

- (void)colorify{
    for (NCIEndpoint *point in [endpoints allValues]){
        point.backgroundColor = [self getColor:point.group];
    }
}

- (void)showActivities{
    isFlows = NO;
    isColorified = NO;
    isPretty = NO;
    isInternal = NO;
    curData = _communityGraphData;
    [self drawPoints];
}

- (void)showPrettyActivities{
    if (isFlows){
        [self showActivities];
    }
    if (isToMuch)
        return;
    if (isInternal){
        for (NCIEndpoint *point in [endpoints allValues]){
            point.backgroundColor =  [self getColor:0];
        }
    }
    isPretty = YES;
    isInternal = NO;
    [self applyForces];
}

- (void)showInternalActivities{
    if (!isPretty){
        [self showPrettyActivities];
    }
    if (isToMuch)
        return;
    isInternal = YES;
    [self showInternal];
}

- (void)drawPoints{
    for (UIView *view in self.subviews){
        [view removeFromSuperview];
    }
    int numOfPoints = 0;
    for (NSDictionary* community in curData){
        numOfPoints += ((NSArray *)community[@"Endpoints"]).count;
    }
    if (numOfPoints > 300){
        isToMuch = YES;
        [self addSubview:infoLabel];
        infoLabel.text = @"Too many endpoints to draw";
        return;
    }
    isToMuch = NO;
    infoLabel.text = @"";
    
    endpoints = [[NSMutableDictionary alloc] init];
    float pointDimention = 12;
    for (int i=0; i< curData.count; i++){
        NSDictionary* community = curData[i];
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

- (void)applyForces{
    [animator removeAllBehaviors];
    UICollisionBehavior *collision = [[UICollisionBehavior alloc]
                                      initWithItems:[endpoints allValues]];
    collision.translatesReferenceBoundsIntoBoundary = YES;
    [animator addBehavior:collision];
    for (int i=0; i< curData.count; i++){
        NSDictionary* community = curData[i];
        for (NSArray *interaction in community[@"Interactions"]){
            UIAttachmentBehavior *attachment = [[UIAttachmentBehavior alloc]
                                                initWithItem:endpoints[interaction[0]]
                                                attachedToItem:endpoints[interaction[1]]];
            [attachment setFrequency:0.0];
            [attachment setDamping:0.0];
            [attachment setLength:100];
            [animator addBehavior:attachment];
        }
    }
    if (curData.count > 1)
        for (int i=0; i< curData.count -1; i++){
            NSDictionary* community1 = curData[i];
            NSDictionary* community2 = curData[i+1];
            NCIEndpoint* ep1 = endpoints[community1[@"Endpoints"][0]];
            NCIEndpoint* ep2 = endpoints[[community2[@"Endpoints"] lastObject]];
            UIAttachmentBehavior *attachment = [[UIAttachmentBehavior alloc]
                                                initWithItem:ep1
                                                attachedToItem:ep2];
            [attachment setFrequency:0.0];
            [attachment setDamping:0.0];
            [attachment setLength:350];
            [animator addBehavior:attachment];
            
        }
    if (curData.count > 2){
        NSDictionary* community1 = curData[0];
        NSDictionary* community2 = [curData lastObject];
        NCIEndpoint* ep1 = endpoints[community1[@"Endpoints"][0]];
        NCIEndpoint* ep2 = endpoints[[community2[@"Endpoints"] lastObject]];
        UIAttachmentBehavior *attachment = [[UIAttachmentBehavior alloc]
                                            initWithItem:ep1
                                            attachedToItem:ep2];
        [attachment setFrequency:0.0];
        [attachment setDamping:0.0];
        [attachment setLength:350];
        [animator addBehavior:attachment];
    }

    [animator performSelector:@selector(removeAllBehaviors) withObject:nil afterDelay:1];
}

- (void)showInternal{
    for (NCIEndpoint *point in [endpoints allValues]){
        if ((point.ip.length > 3 && [[point.ip substringToIndex:3] isEqualToString:@"10."]) ||
            (point.ip.length > 7 && [[point.ip substringToIndex:8] isEqualToString:@"192.168."])) {
            point.backgroundColor = [UIColor lightGrayColor];
        } else {
            point.backgroundColor = [self getColor:point.group];
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
