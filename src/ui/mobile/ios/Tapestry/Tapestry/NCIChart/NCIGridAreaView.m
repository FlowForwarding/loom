//
//  NCIGridAreaView.m
//  Tapestry
//
//  Created by Ira on 11/20/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGridAreaView.h"

@interface NCIGridAreaView(){
    NCIChartView *chart;
    NSMutableArray *xAxisLabels;
    NSDateFormatter* dateFormatter;
    int xLabelShift; //just for UI needs
    
    float xFork;
    float xStep;
    
    float yFork;
    float yStep;
    UILabel *selectedPoint;
    
    float minXVal;
    float maxXVal;
    float minYVal;
    float maxYVal;
}

@end

@implementation NCIGridAreaView

-(id)initWithChart:(NCIChartView *)generalChart{
    self = [self initWithFrame:CGRectZero];
    if (self){
        selectedPoint = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, 7, 7)];
        selectedPoint.backgroundColor = [UIColor greenColor];
        selectedPoint.hidden = YES;
        
        chart = generalChart;
        self.backgroundColor = [UIColor clearColor];
        xLabelShift = 50;

    }
    return self;
}

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:frame];
    if (self) {
        // Initialization code
    }
    return self;
}

- (void)layoutSubviews{
    for (UIView *label in xAxisLabels){
        [label removeFromSuperview];
    }
    [xAxisLabels removeAllObjects];
    
    [self addSubview:selectedPoint];
    int ind;
    int xLabelsCount  = (self.frame.size.width - 150)/150;
    xAxisLabels = [[NSMutableArray alloc] initWithCapacity:xLabelsCount];
    for (ind = 0; ind< xLabelsCount; ind++){
        UILabel *xLabel = [[UILabel alloc] initWithFrame: CGRectZero];
        xLabel.font = [UIFont italicSystemFontOfSize:14];
        // CATransform3D transform = CATransform3DMakeRotation(M_PI/3, 0, 0, 1);
        // xLabel.layer.transform = transform;
        
        [xAxisLabels addObject:xLabel];
        [self addSubview:xLabel];
        dateFormatter = [[NSDateFormatter alloc] init];
        [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm"];
        
    };
    
    for (ind = 0; ind< xAxisLabels.count; ind++){
        UILabel *xLabel = xAxisLabels[ind];
        float xPos = (ind + 0.5)*(self.bounds.size.width)/(xAxisLabels.count - 1) - xLabelShift;
        if (xPos <= self.frame.size.width )
        xLabel.frame = CGRectMake(xPos,
                                  self.bounds.size.height  + 15,
                                  150, 20);
    };
}


//TODO !!!! remove duplication from  NCIGraphView
//temporary made for friday release
- (void)detectXRange {
    minXVal = chart.minXVal;
    maxXVal = chart.maxXVal;
    if (chart.maxYVal - chart.minYVal == 0){
        minYVal = chart.minYVal - 1;
        maxYVal = chart.maxYVal + 1;
    } else {
        minYVal = chart.minYVal + (chart.maxYVal - chart.minYVal)*0.05;
        maxYVal = chart.maxYVal + (chart.maxYVal - chart.minYVal)*0.05;;
    }
}

- (void)drawRect:(CGRect)rect
{
    [self detectXRange];
    xFork = maxXVal - minXVal;
    xStep = self.bounds.size.width/xFork;
    
    yFork = maxYVal - minYVal;
    yStep = self.bounds.size.height/yFork;
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    if (chart.chartData.count > 0){
        [path moveToPoint:[self pointByServerData:chart.chartData[0]]];
    }
    
    int ind;
    for (ind = 1; ind < chart.chartData.count; ind++){
        [path addLineToPoint:[self pointByServerData:chart.chartData[ind]]];
    };
    
    if (chart.chartData.count > 1){
        NSDate *date = chart.chartData[ind-1][0];
        int yVal = self.frame.size.height - 0;
        int xVal =  ([date timeIntervalSince1970] - minXVal)*xStep;
        [path addLineToPoint:CGPointMake(xVal, yVal)];
        
        date = chart.chartData[0][0];
        yVal = self.frame.size.height - (([chart.chartData[0][1] integerValue] - minYVal)*yStep);
        xVal =  ([date timeIntervalSince1970] - minXVal)*xStep;
        [path addLineToPoint:CGPointMake(xVal, self.frame.size.height)];
        
        [[[UIColor blueColor] colorWithAlphaComponent:0.1] setFill];
        [path closePath];
        [path fill];
    }
    
    [[UIColor blueColor] setStroke];
    [path setLineWidth:.5];
    [path stroke];
    
    CGContextRef currentContext = UIGraphicsGetCurrentContext();
    CGContextSetLineWidth(currentContext, 0.5);
    

    CGFloat dashes[] = { 1, 1 };
    CGContextSetLineDash(currentContext, 0.0,  dashes , 2 );
    [[UIColor blackColor] setStroke];
    
    if (maxXVal && minXVal){
        for (ind = 0; ind< xAxisLabels.count; ind++){
            UILabel *xLabel = xAxisLabels[ind];
            NSDate *date = [NSDate dateWithTimeIntervalSince1970:(minXVal + ind * xFork/(xAxisLabels.count - 1))];
            xLabel.text = [NSString stringWithFormat:@"%@", [dateFormatter stringFromDate: date]];
           // if (self.hasGrid || ind == 0){
                CGContextMoveToPoint(currentContext, xLabel.frame.origin.x + xLabelShift, xLabel.frame.origin.y );
                CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x + xLabelShift, 0);
                CGContextStrokePath(currentContext);
           // }
        };
    };
    
}

- (CGPoint)pointByServerData:(NSArray *)data{
    NSDate *date = data[0];
    int yVal = self.frame.size.height - (([data[1] integerValue] - minYVal)*yStep);
    int xVal =  ([date timeIntervalSince1970] - minXVal)*xStep;
    return CGPointMake(xVal, yVal);
}

-(void) touchesBegan:(NSSet *)touches withEvent:(UIEvent *)event
{
    if (_hasPointSelector)
        [self showPoint:event];
}

- (void) touchesMoved:(NSSet *)touches withEvent:(UIEvent *)event{
    if (_hasPointSelector)
        [self showPoint:event];
}

- (void)showPoint:(UIEvent *)event{
    UITouch *touch = [[event allTouches] anyObject];
    CGPoint location = [touch locationInView:self];
    NSDate *date =  [NSDate dateWithTimeIntervalSince1970: location.x/xStep + minXVal];
    int i;
    for (i =0; i < chart.chartData.count; i++){
        NSArray *point = chart.chartData[i];
        if ([date compare:point[0]] == NSOrderedAscending){
            selectedPoint.hidden = NO;
            selectedPoint.center = [self pointByServerData:point];
            chart.selectedPoint.text = [NSString stringWithFormat:@"NCI: %@  %@", point[1],
                                        [dateFormatter stringFromDate:point[0]]];
            return;
        }
    }
    chart.selectedPoint.text = @"";
    selectedPoint.hidden = YES;
}


@end
