//
//  NCIGridAreaView.h
//  Tapestry
//
//  Created by Ira on 11/20/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import <UIKit/UIKit.h>

@class NCIGraphView;

@interface NCIGridAreaView : UIView

-(id)initWithGraph:(NCIGraphView *)graphView;

@property(nonatomic)bool hasPointSelector;

@end
