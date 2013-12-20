//
//  ViewController.m
//  NCIChartConfigurations
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "ViewController.h"
#import "NCIChartView.h"

@interface ViewController (){
    NCIChartView *chart;
    
    float width;
    float heigth;
    float horisontalIndent;
    float varticalIndent;
    
    bool isShowingLandscapeView;
}

@end

@implementation ViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
    horisontalIndent = 20;
    varticalIndent = 20;
    
	chart = [[NCIChartView alloc] initWithFrame:CGRectZero];
    [self.view addSubview:chart];
    
    [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(orientationChanged:)
                                                 name:UIDeviceOrientationDidChangeNotification
                                               object:nil];
    isShowingLandscapeView = NO;
    [self layoutSubviews];
    
}

- (void)layoutSubviews{
    if (!isShowingLandscapeView){
        width = self.view.frame.size.width;
        heigth = self.view.frame.size.height;
    } else {
        width = self.view.frame.size.height;
        heigth = self.view.frame.size.width;
    }
    
    chart.frame = CGRectMake(horisontalIndent,  varticalIndent,
                             width - 2*horisontalIndent,
                             heigth - 2*horisontalIndent);
    [chart drawChart];
    
}

- (void)orientationChanged:(NSNotification *)notification
{
    UIDeviceOrientation deviceOrientation = [UIDevice currentDevice].orientation;
    if (UIDeviceOrientationIsLandscape(deviceOrientation) &&
        !isShowingLandscapeView)
    {
        isShowingLandscapeView = YES;
        [self layoutSubviews];
    }
    else if (isShowingLandscapeView &&
             ((UIDeviceOrientationPortrait == deviceOrientation && UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPhone) ||
              (UIDeviceOrientationIsPortrait(deviceOrientation) && UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad)))
    {
        isShowingLandscapeView = NO;
        [self layoutSubviews];
    }
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Dispose of any resources that can be recreated.
}

@end
