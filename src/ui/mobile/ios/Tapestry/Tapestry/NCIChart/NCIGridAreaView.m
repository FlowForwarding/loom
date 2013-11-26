//
//  NCIGridAreaView.m
//  Tapestry
//
//  Created by Ira on 11/20/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGridAreaView.h"
#import "NCIGraphView.h"

@interface NCIGridAreaView(){
    NCIGraphView *graph;
    NSMutableArray *xAxisLabels;
    NSDateFormatter* dateFormatter;
    int xLabelShift; //just for UI needs
    
    float xFork;
    float xStep;
    
    float yFork;
    float yStep;
    UILabel *selectedPoint;
    NSDate *selectedPointDate;
}

@end

@implementation NCIGridAreaView

-(id)initWithGraph:(NCIGraphView *)graphView{
    self = [self initWithFrame:CGRectZero];
    if (self){
        selectedPoint = [[UILabel alloc] initWithFrame:CGRectMake(0, 0, 7, 7)];
        selectedPoint.backgroundColor = [UIColor greenColor];
        selectedPoint.hidden = YES;
        
        graph = graphView;
        self.backgroundColor = [UIColor clearColor];
        xLabelShift = 50;
        
        dateFormatter = [[NSDateFormatter alloc] init];

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


- (void)drawRect:(CGRect)rect
{
    
    xFork = graph.maxXVal - graph.minXVal;
    xStep = self.bounds.size.width/xFork;
    
    yFork = graph.maxYVal - graph.minYVal;
    yStep = self.bounds.size.height/yFork;
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    if (graph.chart.chartData.count > 0){
        [path moveToPoint:[self pointByServerData:graph.chart.chartData[0]]];
    }
    
    int ind;
    for (ind = 1; ind < graph.chart.chartData.count; ind++){
        [path addLineToPoint:[self pointByServerData:graph.chart.chartData[ind]]];
    };
    
    if (graph.chart.chartData.count > 1){
        NSDate *date = graph.chart.chartData[ind-1][0];
        int yVal = self.frame.size.height - 0;
        int xVal =  ([date timeIntervalSince1970] - graph.minXVal)*xStep;
        [path addLineToPoint:CGPointMake(xVal, yVal)];
        
        date = graph.chart.chartData[0][0];
        yVal = self.frame.size.height - (([graph.chart.chartData[0][1] integerValue] - graph.minYVal)*yStep);
        xVal =  ([date timeIntervalSince1970] - graph.minXVal)*xStep;
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
    
    if (graph.maxXVal && graph.minXVal){
        double step = xFork/(xAxisLabels.count - 1);
        if (step < 60*60*24){
          [dateFormatter setDateFormat:@"yyyy-MMM-dd HH:mm"];
        } else if (step < 60*60*24*30){
           [dateFormatter setDateFormat:@"yyyy-MMM-dd"];
        } else {
            [dateFormatter setDateFormat:@"yyyy-MMM"];
        }
        for (ind = 0; ind< xAxisLabels.count; ind++){
            UILabel *xLabel = xAxisLabels[ind];
            NSDate *date = [NSDate dateWithTimeIntervalSince1970:(graph.minXVal + ind * step)];
            NSString *text = graph.chart.chartData.count == 0 ? @"" :  [NSString stringWithFormat:@"%@", [dateFormatter stringFromDate: date]];
            xLabel.text = text;
           // if (self.hasGrid || ind == 0){
                CGContextMoveToPoint(currentContext, xLabel.frame.origin.x + xLabelShift, xLabel.frame.origin.y );
                CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x + xLabelShift, 0);
                CGContextStrokePath(currentContext);
           // }
        };
    };
    [self layoutSelectedPoint];
    
}

- (CGPoint)pointByServerData:(NSArray *)data{
    NSDate *date = data[0];
    int yVal = self.frame.size.height - (([data[1] integerValue] - graph.minYVal)*yStep);
    int xVal =  ([date timeIntervalSince1970] - graph.minXVal)*xStep;
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
    selectedPointDate =  [NSDate dateWithTimeIntervalSince1970: location.x/xStep + graph.minXVal];
    [self layoutSelectedPoint];
}

- (void)layoutSelectedPoint{
    if (!selectedPointDate)
        return;
    int i;
    for (i =0; i < graph.chart.chartData.count; i++){
        NSArray *point = graph.chart.chartData[i];
        if ([selectedPointDate compare:point[0]] == NSOrderedAscending){
            selectedPoint.hidden = NO;
            selectedPoint.center = [self pointByServerData:point];
            [dateFormatter setDateFormat:@"yyyy-MMM-dd HH:mm:ss"];
            graph.chart.selectedPoint.text = [NSString stringWithFormat:@"NCI: %@  %@", point[1],
                                              [dateFormatter stringFromDate:point[0]]];
            return;
        }
    }
    graph.chart.selectedPoint.text = @"";
    selectedPoint.hidden = YES;
    selectedPointDate = nil;

}


@end
