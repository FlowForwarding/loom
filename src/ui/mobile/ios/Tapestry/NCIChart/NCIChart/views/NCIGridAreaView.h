//
//  NCIGridAreaView.h
//  Tapestry
//
//  Created by Infoblox Inc on 11/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//  Licensed under the Apache License, Version 2.0 
//  http://www.apache.org/licenses/LICENSE-2.0
//

#import <UIKit/UIKit.h>

@class NCIGraphView;

@interface NCIGridAreaView : UIView

-(id)initWithGraph:(NCIGraphView *)graphView;

@property(nonatomic)bool hasPointSelector;

@end
