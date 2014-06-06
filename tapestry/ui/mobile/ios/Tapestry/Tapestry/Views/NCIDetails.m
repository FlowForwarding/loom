//
//  NCIDetails.m
//  Tapestry
//
//  Created by Ira on 5/16/14.
//  Copyright (c) 2014 FlowForwarding.Org. All rights reserved.
//

#import "NCIDetails.h"

@implementation NCIDetails

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        self.contentSize = CGSizeMake(self.frame.size.width, self.frame.size.height*2);
        _content = [[UIView alloc] initWithFrame:CGRectMake(0, self.frame.size.height, self.frame.size.width, self.frame.size.height)];
        [self addSubview:_content];
        _content.backgroundColor = [UIColor whiteColor];
        _content.layer.cornerRadius = 5;
        self.pagingEnabled = YES;
        self.delegate = self;
        self.layer.cornerRadius = 5;
        self.showsVerticalScrollIndicator = NO;
    }
    return self;
}

- (void)scrollViewDidScroll:(UIScrollView *)scrollView{
    if (scrollView.contentOffset.y <= 0){
        self.center = CGPointMake(self.center.x, -self.frame.size.height/2);
        [self hideActions];
    } else if (scrollView.contentOffset.y > scrollView.frame.size.height){
        [scrollView setContentOffset:CGPointMake(0, scrollView.frame.size.height)];
    }
}

- (void)hideActions{
    
}

@end
