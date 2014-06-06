//
//  NCIDetails.h
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import <UIKit/UIKit.h>

@interface NCIDetails : UIScrollView<UIScrollViewDelegate>

@property(nonatomic, strong)UIView *content;

- (void)hideActions;

@end
