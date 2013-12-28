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
    UIScrollView *book;
    NCIChartView *nciChart;
    NCISimpleChartView *simpleChart;
    UIPageControl *pager;
    
    float horisontalIndent;
    float verticalIndent;
    float pagerHeigth;
    
    int numberOfPages;
    
    bool isShowingLandscapeView;
}

@end

@implementation ViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
    
    if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad){
        horisontalIndent = 20;
        verticalIndent = 40;
        pagerHeigth = 100;
    } else {
        horisontalIndent = 10;
        verticalIndent = 20;
        pagerHeigth = 40;
    }
    
    numberOfPages = 3;
    
    book = [[UIScrollView alloc] initWithFrame:CGRectZero];
    book.scrollEnabled = NO;
    [self.view addSubview:book];
    
    nciChart = [[NCIChartView alloc] initWithFrame:CGRectZero];
    [book addSubview:nciChart];
    
    simpleChart = [[NCISimpleChartView alloc] initWithFrame:CGRectZero
                                                 andOptions:@{nciIsFill: @(NO),
                                                              nciLineColor : [UIColor greenColor],
                                                              nciLineWidth : @(2),
                                                              nciSelPointImage : @"star",
                                                              nciSelPointSize: @(20),
                                                              nciXLabelsFont: [UIFont fontWithName:@"MarkerFelt-Thin" size:12],
                                                              nciYLabelsFont: [UIFont fontWithName:@"MarkerFelt-Thin" size:12],
                                                              nciSelPointFont: [UIFont fontWithName:@"MarkerFelt-Thin" size:14],
                                                              nciYLabelsDistance: @(50),
                                                              nciXLabelsDistance: @(80)}];
    [book addSubview:simpleChart];
    
    [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(orientationChanged:)
                                                 name:UIDeviceOrientationDidChangeNotification
                                               object:nil];
    isShowingLandscapeView = NO;
    
    [self generateDemoData];
    
    pager = [[UIPageControl alloc] initWithFrame:CGRectZero];
    pager.numberOfPages =  numberOfPages;
    [pager addTarget:self action:@selector(changePage) forControlEvents:UIControlEventValueChanged];
    [pager setPageIndicatorTintColor:[UIColor blackColor]];
    [pager setCurrentPageIndicatorTintColor:[UIColor redColor]];
    [self.view addSubview:pager];
    
    [self layoutSubviews];
    
}

- (void)changePage{
    [book setContentOffset:CGPointMake(pager.currentPage*book.frame.size.width, 0) animated:YES];
}

- (void)layoutSubviews{
    float width;
    float heigth;
    if (!isShowingLandscapeView){
        width = self.view.frame.size.width;
        heigth = self.view.frame.size.height;
    } else {
        width = self.view.frame.size.height;
        heigth = self.view.frame.size.width;
    }
    
    book.frame = CGRectMake(0,  verticalIndent, width, heigth - verticalIndent  - pagerHeigth);
    book.contentSize = CGSizeMake(width*numberOfPages, heigth  - verticalIndent  - pagerHeigth);
    
    nciChart.frame = CGRectMake(horisontalIndent, 0,
                             width - 2*horisontalIndent,
                             heigth - verticalIndent - pagerHeigth);
    
    simpleChart.frame = CGRectMake(horisontalIndent +  width, 0,
                                width - 2*horisontalIndent,
                                heigth - verticalIndent - pagerHeigth);
    
    pager.frame = CGRectMake(0, heigth - pagerHeigth, width, pagerHeigth);
    

}

- (void)generateDemoData{
    float halfYearPeriod = 60*60*24*30*6;
    float demoDatePeriod = halfYearPeriod;
    float numOfPoints = 50;
    float step = demoDatePeriod/(numOfPoints - 1);
    int trendMiddle = 6;
    int trendStepCounter = 0;
    int ind;
    for (ind = 0; ind < numOfPoints; ind ++){
        if (trendStepCounter > 20){
            trendStepCounter = 0;
            trendMiddle += 1;
        }
        trendStepCounter += 1;
        int value = trendMiddle + arc4random() % 5;
        //NSDate *date = [[NSDate date] dateByAddingTimeInterval: (-demoDatePeriod + step*ind)];
        double time = [[NSDate date] timeIntervalSince1970] - demoDatePeriod + step*ind;
        if (trendStepCounter > 4 && trendStepCounter < 7){
            [nciChart addPoint:time val: nil];
            [simpleChart addPoint:time val:nil];
        } else {
            [nciChart addPoint:time val: @(value)];
            [simpleChart addPoint:time val: @(value)];
        }

    }
    
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
