//
//  NCISimpleChartView.m
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import "NCISimpleChartView.h"
#import "NCISimpleGridView.h"

@interface NCISimpleChartView(){
    NSDateFormatter* dateFormatter;
    NSDate *selectedPointDate;
    UIView *selectedPoint;
}

@end

@implementation NCISimpleChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        _hasYLabels = YES;
        _topBottomGridSpace = 10;
        dateFormatter = [[NSDateFormatter alloc] init];
        self.backgroundColor = [UIColor clearColor];
        self.chartData = [[NSMutableArray alloc] init];
        [self addSubviews];
    }
    return self;
}

- (void)addSubviews{
    _graph = [[NCISimpleGraphView alloc] initWithChart:self];
    [self addSubview:_graph];
    if (_hasSelection){
        [self setupSelection];
    }
}

- (void)setupSelection{
    _selectedLabel = [[UILabel alloc] initWithFrame:CGRectZero];
    _selectedLabel.font = [UIFont boldSystemFontOfSize:18];
    
    selectedPoint = [[UIView alloc] initWithFrame:CGRectMake(0, 0, 8, 8)];
    selectedPoint.backgroundColor = [UIColor blueColor];//[UIColor tapestryDarkBlue];
    selectedPoint.layer.cornerRadius = 4;
    selectedPoint.hidden = YES;
    [self addSubview:selectedPoint];
    
    [self addSubview:_selectedLabel];
    UITapGestureRecognizer *gridTapped = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(gridTapped:)];
    gridTapped.numberOfTapsRequired = 1;
    [self.graph.grid addGestureRecognizer:gridTapped];
}

- (void)setHasSelection:(bool)hasSelection{
    _hasSelection = YES;
    [self setupSelection];
}

- (void)gridTapped:(UITapGestureRecognizer *)recognizer{
    CGPoint location = [recognizer locationInView:self];
    selectedPointDate = [self.graph getDateByX:location.x];
    [self layoutSelectedPoint];
}

- (void)layoutSelectedPoint{
    if (!selectedPointDate)
        return;
    for (int i =0; i < _chartData.count; i++){
        NSArray *point = _chartData[i];
        if ([selectedPointDate compare:point[0]] == NSOrderedAscending){
            selectedPoint.hidden = NO;
            CGPoint pointInGraph = [self.graph pointByServerDataInGrid:point];
            selectedPoint.center =  CGPointMake(pointInGraph.x + self.graph.xLabelsWidth, pointInGraph.y);
            [dateFormatter setDateFormat:@"yyyy-MMM-dd HH:mm:ss"];
            _selectedLabel.text = [NSString stringWithFormat:@"NCI: %@  %@", point[1],
                                              [dateFormatter stringFromDate:point[0]]];
            return;
        }
    }
    _selectedLabel.text = @"";
    selectedPoint.hidden = YES;
    selectedPointDate = nil;
    
}

- (void)layoutSubviews{
    float labelHeight = 20;
    if (_hasSelection){
        _selectedLabel.frame = CGRectMake(self.bounds.size.width - 320, 0, 300, labelHeight);
        [self layoutSelectedPoint];
    }
    _graph.frame = self.bounds;
    [_graph layoutSubviews];
}

- (void)addPoint:(NSDate *)date val:(NSNumber *)value{
    if (value == nil){
        [self.chartData addObject:@[date, [NSNull null]]];
    } else {
        [self.chartData addObject:@[date, value]];
    }
}

- (NSArray *)getBoundaryValues{
    float minY = MAXFLOAT;
    float maxY = -MAXFLOAT;
    for (NSArray *point in self.chartData){
        if ([point[1] isKindOfClass:[NSNull class]]){
            continue;
        }
        float val = [point[1] floatValue];
        if (val < minY){
            minY = val;
        }
        if (val > maxY){
            maxY = val;
        }
    }
    float diff = (maxY - minY);
    if (diff == 0){
        maxY = maxY + 1;
        minY = minY - 1;
    }
    return @[@(minY - diff*_topBottomGridSpace/100), @(maxY + diff*_topBottomGridSpace/100)];
}

@end
