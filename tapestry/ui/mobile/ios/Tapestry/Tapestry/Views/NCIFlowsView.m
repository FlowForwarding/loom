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
}
@end

@implementation NCIFlowsView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        UILabel *label = [[UILabel alloc] initWithFrame:self.bounds];
        label.textAlignment = NSTextAlignmentCenter;
        label.text = @"Connetions graph here";
        [self addSubview:label];
        animator = [[UIDynamicAnimator alloc] initWithReferenceView:self];
    }
    return self;
}

- (void)loadData:(NSArray *) communities{
    groupColors = [[NSMutableArray alloc] initWithArray:@[[UIColor blueColor], [UIColor greenColor], [UIColor purpleColor]]];
    endpoints = [[NSMutableDictionary alloc] init];
    float pointDimention = 12;
    for (int i=0; i< communities.count; i++){
        NSDictionary* community = communities[i];
        for (NSString *ePoint in community[@"Endpoints"]){
            NCIEndpoint *ep = [[NCIEndpoint alloc] initWithFrame:
                               CGRectMake(arc4random() % (int)self.frame.size.width,
                                          pointDimention + arc4random() % (int)self.frame.size.height - 2*pointDimention,
                                          pointDimention, pointDimention)];
            ep.group = i;
            ep.backgroundColor = [self getColor:i];
            [self addSubview:ep];
            endpoints[ePoint] = ep;
        }
    }
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
