//
//  ViewController.m
//  NCIChartConfigurations
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "ViewController.h"
#import "NCIRangesChartView.h"
#import "NCIChartView.h"

@interface ViewController (){
    UIScrollView *book;
    NCIChartView *chart;
    NCIRangesChartView *rangesChart;
    
    float horisontalIndent;
    float varticalIndent;
    
    int numberOfPages;
    
    bool isShowingLandscapeView;
}

@end

@implementation ViewController

- (void)viewDidLoad
{
    [super viewDidLoad];
    horisontalIndent = 20;
    varticalIndent = 40;
    numberOfPages = 1;
    
    book = [[UIScrollView alloc] initWithFrame:CGRectZero];
    book.pagingEnabled = YES;
    [self.view addSubview:book];
    
    chart = [[NCIChartView alloc] initWithFrame:CGRectZero];
    [book addSubview:chart];
    
//    rangesChart = [[NCIRangesChartView alloc] initWithFrame:CGRectZero];
//    [book addSubview:rangesChart];
    
    [[UIDevice currentDevice] beginGeneratingDeviceOrientationNotifications];
    [[NSNotificationCenter defaultCenter] addObserver:self
                                             selector:@selector(orientationChanged:)
                                                 name:UIDeviceOrientationDidChangeNotification
                                               object:nil];
    isShowingLandscapeView = NO;
    
    [self generateDemoData];
    [self layoutSubviews];
    
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
    
    book.frame = CGRectMake(0,  0, width, heigth);
    book.contentSize = CGSizeMake(width*numberOfPages, heigth);
    
    chart.frame = CGRectMake(horisontalIndent, varticalIndent,
                             width - 2*horisontalIndent,
                             heigth - 2*varticalIndent);
    
    rangesChart.frame = CGRectMake(width + horisontalIndent,  varticalIndent,
                             width - 2*horisontalIndent,
                             heigth - 2*varticalIndent);

}

- (void)generateDemoData{
    float halfYearPeriod = 60*60*24*30*6;
    float demoDatePeriod = halfYearPeriod;
    float numOfPoints = 800;
    float step = demoDatePeriod/(numOfPoints - 1);
    int trendMiddle = 6;
    int trendStepCounter = 0;
    int ind;
    for (ind = 0; ind < numOfPoints; ind ++){
        if (trendStepCounter > 5){
            trendStepCounter = 0;
            trendMiddle += 1;
        }
        trendStepCounter += 1;
        int value = trendMiddle + arc4random() % 5;

        NSDate *date = [[NSDate date] dateByAddingTimeInterval: (-demoDatePeriod + step*ind)];
        [rangesChart addPoint:date val: [@(value) description]];
        [chart addPoint:date val: [@(value) description]];

    }
    
    if (!rangesChart.minRangeDate){
        long period = [[NSDate date] timeIntervalSince1970] - [((NSDate *)[rangesChart.chartData firstObject][0]) timeIntervalSince1970];
        rangesChart.minRangeDate = [[NSDate date] dateByAddingTimeInterval: - period /10.0];
        rangesChart.maxRangeDate = [NSDate date];
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
