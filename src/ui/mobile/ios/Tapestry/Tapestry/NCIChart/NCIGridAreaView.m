//
//  NCIGridAreaView.m
//  Tapestry
//
//  Created by Ira on 11/20/13.
//  Copyright (c) 2013 Truststix. All rights reserved.
//

#import "NCIGridAreaView.h"
#import "NCIGraphView.h"
#import "NCIBottomGraphView.h"

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
        [self addSubview:selectedPoint];
        
        graph = graphView;
        self.backgroundColor = [UIColor clearColor];
        xLabelShift = 50;
        
        dateFormatter = [[NSDateFormatter alloc] init];
        
        int xLabelsCount  = 20;
        xAxisLabels = [[NSMutableArray alloc] initWithCapacity:xLabelsCount];
        for (int ind = 0; ind< xLabelsCount; ind++){
            UILabel *xLabel = [[UILabel alloc] initWithFrame: CGRectZero];
            xLabel.font = [UIFont italicSystemFontOfSize:14];
            // CATransform3D transform = CATransform3DMakeRotation(M_PI/3, 0, 0, 1);
            // xLabel.layer.transform = transform;
            [xAxisLabels addObject:xLabel];
            [self addSubview:xLabel];
        };
        
        //if (_hasPointSelector)
        UITapGestureRecognizer *tapRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(tapped:)];
        tapRecognizer.numberOfTouchesRequired = 1;
        [self addGestureRecognizer:tapRecognizer];

    }
    return self;
}

-(void)tapped:(UITapGestureRecognizer *) gesture{
    CGPoint location = [gesture locationInView:self];
    selectedPointDate =  [NSDate dateWithTimeIntervalSince1970: location.x/xStep + [graph.chart.minRangeDate timeIntervalSince1970]];
    [self layoutSelectedPoint];
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
    
}

- (void)drawRect:(CGRect)rect
{
    xFork = graph.maxXVal - graph.minXVal;
    xStep = graph.gridScroll.contentSize.width/xFork;
    
    yFork = graph.maxYVal - graph.minYVal;
    yStep = self.bounds.size.height/yFork;
    
    UIBezierPath *path = [UIBezierPath bezierPath];
    if (graph.chart.chartData.count > 0){
        [path moveToPoint:[self pointByServerData:graph.chart.chartData[graph.minDataIndex]]];
    }
    
    long ind;
    if (graph.chart.chartData.count > 0){
        for (ind = graph.minDataIndex; ind <= graph.maxDataIndex; ind++){
            [path addLineToPoint:[self pointByServerData:graph.chart.chartData[ind]]];
        };
    }
    
    if (graph.chart.chartData.count > 1){
        NSDate *date = graph.chart.chartData[ind-1][0];
        double yVal = self.frame.size.height - 0;
        double xVal =  ([date timeIntervalSince1970] - graph.minXVal)*xStep;
        if (graph.scaleIndex > 1){
            xVal =  ([date timeIntervalSince1970] - [graph.chart.minRangeDate timeIntervalSince1970])*xStep;
        }
        [path addLineToPoint:CGPointMake(xVal, yVal)];
        
        date = graph.chart.chartData[graph.minDataIndex][0];
        yVal = self.frame.size.height - (([graph.chart.chartData[graph.minDataIndex][1] integerValue] - graph.minYVal)*yStep);
        xVal =  ([date timeIntervalSince1970] - graph.minXVal)*xStep;
        if (graph.scaleIndex > 1){
            xVal =  ([date timeIntervalSince1970] - [graph.chart.minRangeDate timeIntervalSince1970])*xStep;
        }
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
    
    long xImplicitLabelsCount  = (graph.gridScroll.contentSize.width - 150)/150;
    double step = xFork/(xImplicitLabelsCount + 1);
    if (step < 60*60*24){
        [dateFormatter setDateFormat:@"yyyy-MMM-dd HH:mm"];
    } else if (step < 60*60*24*30){
        [dateFormatter setDateFormat:@"yyyy-MMM-dd"];
    } else {
        [dateFormatter setDateFormat:@"yyyy-MMM"];
    }

    if (graph.maxXVal && graph.minXVal){
        int curRealIndex = 0;
        for (ind = 0; ind< xImplicitLabelsCount; ind++){
            long timeInterval = graph.minXVal + ind * step;
            if ((graph.scaleIndex == 1) || (timeInterval >= [graph.chart.minRangeDate timeIntervalSince1970]  &&
                timeInterval <= [graph.chart.maxRangeDate timeIntervalSince1970])){
                
                NSDate *date = [NSDate dateWithTimeIntervalSince1970:timeInterval];
                UILabel *xLabel = xAxisLabels[curRealIndex];
                NSString *text = graph.chart.chartData.count == 0 ? @"" :  [NSString stringWithFormat:@"%@", [dateFormatter stringFromDate: date]];
                xLabel.text = text;
                float xPosImplicit = (ind + 0.5)*(graph.gridScroll.contentSize.width)/(xImplicitLabelsCount) - xLabelShift;
                float xPos = xPosImplicit - self.frame.origin.x;
                if (xPos <= self.frame.size.width )
                    xLabel.frame = CGRectMake(xPos,
                                              self.bounds.size.height  + 15,
                                              150, 20);
                curRealIndex ++;
            }
        };
        
        for (ind = curRealIndex; ind < xAxisLabels.count; ind++){
             UILabel *xLabel = xAxisLabels[ind];
             xLabel.text = @"";
        };
        
        for (ind = 0; ind< xAxisLabels.count; ind++){
            UILabel *xLabel = xAxisLabels[ind];
            // if (self.hasGrid || ind == 0){
            CGContextMoveToPoint(currentContext, xLabel.frame.origin.x + xLabelShift, xLabel.frame.origin.y );
            CGContextAddLineToPoint(currentContext, xLabel.frame.origin.x + xLabelShift, 0);
            // }
        };
    };
    
    CGContextStrokePath(currentContext);
    [self layoutSelectedPoint];
    
}

- (CGPoint)pointByServerData:(NSArray *)data{
    NSDate *date = data[0];
    float yVal = self.frame.size.height - (([data[1] integerValue] - graph.minYVal)*yStep);
    
    float xVal =  ([date timeIntervalSince1970] - graph.minXVal)*xStep;
    if (graph.scaleIndex > 1){
        xVal =  ([date timeIntervalSince1970] - [graph.chart.minRangeDate timeIntervalSince1970])*xStep;
    }
    return CGPointMake(xVal, yVal);
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
