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
    float labelHeight;
}


@end


@implementation NCISimpleChartView

- (id)initWithFrame:(CGRect)frame
{
    self = [super initWithFrame:(CGRect)frame];
    if (self) {
        labelHeight = 0;
        _hasYLabels = YES;
        _nciIsFill = YES;
        _topBottomGridSpace = 10;
        _nciLineWidth = 0.3;
        _nciLineColor = [UIColor blueColor];
        _nciSelPointColor = [UIColor blueColor];
        _nciSelPointSize = 8;
        
        if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad){
            _nciXLabelsFont = [UIFont italicSystemFontOfSize:12];
            _nciYLabelsFont = [UIFont systemFontOfSize:12];
            _nciSelPointFont = [UIFont boldSystemFontOfSize:18];
            _nciXLabelsDistance = 200;
            _nciYLabelsDistance = 80;
        } else {
            _nciXLabelsFont = [UIFont italicSystemFontOfSize:10];
            _nciYLabelsFont = [UIFont systemFontOfSize:10];
            _nciSelPointFont = [UIFont boldSystemFontOfSize:14];
            _nciXLabelsDistance = 100;
            _nciYLabelsDistance = 40;
        }
        
        dateFormatter = [[NSDateFormatter alloc] init];
        self.backgroundColor = [UIColor clearColor];
        self.chartData = [[NSMutableArray alloc] init];
        [self addSubviews];
    }
    return self;
}

-(id)initWithFrame:(CGRect)frame andOptions:(NSDictionary *)opts{
    self = [self initWithFrame:frame];
    if (self){
        
        if ([opts objectForKey:nciIsFill])
            _nciIsFill = [[opts objectForKey:nciIsFill] boolValue];
        
        for (NSString* key in @[nciLineColor, nciXLabelsFont, nciYLabelsFont, nciSelPointFont]){
            [self setValue:[opts objectForKey:key] forKey:key];
        }
        
        if ([opts objectForKey:nciLineWidth])
            _nciLineWidth = [[opts objectForKey:nciLineWidth] floatValue];
        if ([opts objectForKey:nciXLabelsDistance])
            _nciXLabelsDistance = [[opts objectForKey:nciXLabelsDistance] floatValue];
        if ([opts objectForKey:nciYLabelsDistance])
            _nciYLabelsDistance = [[opts objectForKey:nciYLabelsDistance] floatValue];
        
        if ([opts objectForKey:nciHasSelection])
            _nciHasSelection = [[opts objectForKey:nciLineWidth] boolValue];
        if ([opts objectForKey:nciSelPointColor]){
            _nciSelPointColor = [opts objectForKey:nciSelPointColor];
            _nciHasSelection = YES;
        }
        if ([opts objectForKey:nciSelPointSize]){
            _nciSelPointSize = [[opts objectForKey:nciSelPointSize] floatValue];
            _nciHasSelection = YES;
        }
        if ([opts objectForKey:nciSelPointImage]){
            _nciSelPointImage = [opts objectForKey:nciSelPointImage];
            _nciHasSelection = YES;
        }
        self.nciHasSelection = _nciHasSelection;

    }
    return self;
}

- (void)addSubviews{
    _graph = [[NCISimpleGraphView alloc] initWithChart:self];
    [self addSubview:_graph];
    if (_nciHasSelection){
        [self setupSelection];
    }
}

- (void)setupSelection{
    if (_selectedLabel)
        return;
    _selectedLabel = [[UILabel alloc] initWithFrame:CGRectZero];
    _selectedLabel.font = _nciSelPointFont;
    
    if (_nciSelPointImage){
        selectedPoint = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, _nciSelPointSize, _nciSelPointSize)];
        ((UIImageView *)selectedPoint).image = [UIImage imageNamed:_nciSelPointImage];
    } else {
        selectedPoint = [[UIView alloc] initWithFrame:CGRectMake(0, 0, _nciSelPointSize, _nciSelPointSize)];
        selectedPoint.backgroundColor = _nciSelPointColor;//[UIColor tapestryDarkBlue];
        selectedPoint.layer.cornerRadius = _nciSelPointSize/2;
    }
    selectedPoint.hidden = YES;
    [self addSubview:selectedPoint];
    
    [self addSubview:_selectedLabel];
    UITapGestureRecognizer *gridTapped = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(gridTapped:)];
    gridTapped.numberOfTapsRequired = 1;
    [self.graph.grid addGestureRecognizer:gridTapped];
}

- (void)setNciHasSelection:(bool)hasSelection{
    _nciHasSelection = YES;
    [self setupSelection];
}

- (void)gridTapped:(UITapGestureRecognizer *)recognizer{
    CGPoint location = [recognizer locationInView:self.graph.grid];
    selectedPointDate = [self.graph getDateByX:location.x];
    [self layoutSelectedPoint];
}

- (void)layoutSelectedPoint{
    if (!selectedPointDate)
        return;
    for (int i =0; i < _chartData.count; i++){
        NSArray *point = _chartData[i];
        if ([selectedPointDate compare:point[0]] <= NSOrderedSame){
            if([point[1] isKindOfClass:[NSNull class]]){
                return;
            }
            selectedPoint.hidden = NO;
            CGPoint pointInGrid = [self.graph pointByServerDataInGrid:point];
            selectedPoint.center =  CGPointMake(pointInGrid.x + self.graph.xLabelsWidth, pointInGrid.y + labelHeight);
            [dateFormatter setDateFormat:@"yyyy-MMM-dd HH:mm:ss"];
            _selectedLabel.text = [NSString stringWithFormat:@"NCI: %@  %@", point[1],
                                   [dateFormatter stringFromDate:point[0]]];
            
            if (pointInGrid.x < 0 || pointInGrid.x >= (self.graph.grid.frame.size.width + 2)){
                selectedPoint.hidden = YES;
            } else {
                selectedPoint.hidden = NO;
            }
            return;
        }
    }
    _selectedLabel.text = @"";
    selectedPoint.hidden = YES;
    selectedPointDate = nil;
    
}

- (void)layoutSubviews{
    labelHeight = 20;
    if (_nciHasSelection){
        _selectedLabel.frame = CGRectMake(self.bounds.size.width - 280, 0, 280, labelHeight);
        [self layoutSelectedPoint];
        _graph.frame = CGRectMake(0, labelHeight, self.bounds.size.width, self.bounds.size.height - labelHeight) ;//self.bounds;
    } else {
        _graph.frame = self.bounds;
    }
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
