//
//  NCIEditServerView.h
//  Tapestry
//
//  Created by Infoblox Inc on 11/18/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import <UIKit/UIKit.h>

@interface NCIEditServerView : UIView

@property(nonatomic, strong) UIWebView *mainView;

- (id)initWithFrame:(CGRect)frame andView:(UIWebView *)webView;

@end
