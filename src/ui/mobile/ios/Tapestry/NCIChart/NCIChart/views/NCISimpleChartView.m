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
    double selectedPointArgument;
    NSMutableArray *selectedPoints;
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
        _nciLineColors = [NSMutableArray arrayWithArray: @[[UIColor blueColor], [UIColor greenColor], [UIColor purpleColor]]];
        _nciSelPointColors = [NSMutableArray arrayWithArray: @[[UIColor blueColor], [UIColor greenColor], [UIColor purpleColor]]];
        selectedPointArgument = NAN;
        
        if (UI_USER_INTERFACE_IDIOM() == UIUserInterfaceIdiomPad){
            _nciXLabelsFont = [UIFont italicSystemFontOfSize:14];
            _nciYLabelsFont = [UIFont systemFontOfSize:14];
            _nciSelPointFont = [UIFont boldSystemFontOfSize:18];
            _nciXLabelsDistance = 200;
            _nciYLabelsDistance = 80;
            _nciSelPointSize = 8;
        } else {
            _nciXLabelsFont = [UIFont italicSystemFontOfSize:10];
            _nciYLabelsFont = [UIFont systemFontOfSize:10];
            _nciSelPointFont = [UIFont boldSystemFontOfSize:12];
            _nciXLabelsDistance = 100;
            _nciYLabelsDistance = 40;
            _nciSelPointSize = 4;
        }
        
        dateFormatter = [[NSDateFormatter alloc] init];
        [dateFormatter setDateFormat:@"yyyy-MMM-dd HH:mm:ss"];
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
        
        for (NSString* key in @[nciLineColors, nciXLabelsFont, nciYLabelsFont, nciSelPointFont]){
            if ([opts objectForKey:key]){
                [self setValue:[opts objectForKey:key] forKey:key];
            }
        }
        
        if ([opts objectForKey:nciLineWidth])
            _nciLineWidth = [[opts objectForKey:nciLineWidth] floatValue];
        if ([opts objectForKey:nciXLabelsDistance])
            _nciXLabelsDistance = [[opts objectForKey:nciXLabelsDistance] floatValue];
        if ([opts objectForKey:nciYLabelsDistance])
            _nciYLabelsDistance = [[opts objectForKey:nciYLabelsDistance] floatValue];
        
        if ([opts objectForKey:nciHasSelection])
            _nciHasSelection = [[opts objectForKey:nciLineWidth] boolValue];
        if ([opts objectForKey:nciSelPointColors]){
            _nciSelPointColors = [opts objectForKey:nciSelPointColors];
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
        if ([opts objectForKey:nciSelPointTextRenderer]){
            _nciSelPointTextRenderer = [opts objectForKey:nciSelPointTextRenderer];
            _nciHasSelection = YES;
        }
        self.nciHasSelection = _nciHasSelection;
        
        if ([opts objectForKey:nciXLabelRenderer]){
            _nciXLabelRenderer = [opts objectForKey:nciXLabelRenderer];
        }
        if ([opts objectForKey:nciYLabelRenderer]){
            _nciYLabelRenderer = [opts objectForKey:nciYLabelRenderer];
        }
        if ([opts objectForKey:nciTapGridAction]){
            _nciTapGridAction = [opts objectForKey:nciTapGridAction];
        }

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

-(UIView *)createSelPoint{
    UIView *selectedPoint;
    if (_nciSelPointImage){
        selectedPoint = [[UIImageView alloc] initWithFrame:CGRectMake(0, 0, _nciSelPointSize, _nciSelPointSize)];
        ((UIImageView *)selectedPoint).image = [UIImage imageNamed:_nciSelPointImage];
    } else {
        selectedPoint = [[UIView alloc] initWithFrame:CGRectMake(0, 0, _nciSelPointSize, _nciSelPointSize)];
        selectedPoint.layer.cornerRadius = _nciSelPointSize/2;
    }
    selectedPoint.hidden = YES;
    [self addSubview:selectedPoint];
    [selectedPoints addObject:selectedPoint];
    return selectedPoint;
}

- (void)setupSelection{
    if (_selectedLabel)
        return;
    _selectedLabel = [[UILabel alloc] initWithFrame:CGRectZero];
    _selectedLabel.font = _nciSelPointFont;
    _selectedLabel.textAlignment = NSTextAlignmentRight;
    _selectedLabel.numberOfLines = 0;
    [self addSubview:_selectedLabel];
    selectedPoints = [[NSMutableArray alloc] init];
    
    UITapGestureRecognizer *gridTapped = [[UITapGestureRecognizer alloc] initWithTarget:self action:@selector(gridTapped:)];
    gridTapped.numberOfTapsRequired = 1;
    [self.graph.grid addGestureRecognizer:gridTapped];
}

- (void)setNciHasSelection:(bool)hasSelection{
    if (hasSelection){
        labelHeight = 20;
        _selectedLabel.hidden = NO;
    } else {
        labelHeight = 0;
        _selectedLabel.hidden = YES;
    }
    _nciHasSelection = hasSelection;
    [self setupSelection];
}

- (void)gridTapped:(UITapGestureRecognizer *)recognizer{
    CGPoint location = [recognizer locationInView:self.graph.grid];
    selectedPointArgument = [self.graph getArgumentByX:location.x];
    if (self.nciTapGridAction){
        self.nciTapGridAction([self.graph getArgumentByX:location.x], [self.graph getValByY:location.y]);
    }
    [self layoutSelectedPoint];
}

- (void)layoutSelectedPoint{
    if (selectedPointArgument != selectedPointArgument)
        return;
    for (int i =0; i < _chartData.count; i++){
        NSArray *point = _chartData[i];
        if (selectedPointArgument <= [point[0] doubleValue] ){
            for (int j = 0; j < ((NSArray *)point[1]).count; j++){
                id val = point[1][j];
                if ([val isKindOfClass:[NSNull class]])
                    continue;
                UIView *selectedPoint;
                if (selectedPoints.count < (j+1)){
                    selectedPoint = [self createSelPoint];
                    if (!_nciSelPointImage)
                        selectedPoint.backgroundColor =  _nciSelPointColors[j];
                } else {
                    selectedPoint = selectedPoints[j];
                }
                selectedPoint.hidden = NO;
                CGPoint pointInGrid = [self.graph pointByServerDataInGrid:@[point[0], val]];
                selectedPoint.center =  CGPointMake(pointInGrid.x + self.graph.xLabelsWidth, pointInGrid.y + labelHeight);
                if (pointInGrid.x < 0 || pointInGrid.x >= (self.graph.grid.frame.size.width + 2)){
                    selectedPoint.hidden = YES;
                } else {
                    selectedPoint.hidden = NO;
                }
            }
            
            if (self.nciSelPointTextRenderer){
                _selectedLabel.text = self.nciSelPointTextRenderer([point[0] doubleValue], point[1]);
            } else {
                NSMutableString *values = [[NSMutableString alloc] init];
                for (id val in point[1]){
                    if (![val isKindOfClass:[NSNull class]]){
                        [values appendString:[val description]];
                        [values appendString:@","];
                    }
                }
                _selectedLabel.text = [NSString stringWithFormat:@"x: %@  y:%@", values,
                                       [dateFormatter stringFromDate:[NSDate dateWithTimeIntervalSince1970:[point[0] doubleValue]]]];
            }
            return;
        }
    }
    for (UIView *selectedPoint in selectedPoints){
        _selectedLabel.text = @"";
        selectedPoint.hidden = YES;
    }
    selectedPointArgument = NAN;
}

- (void)layoutSubviews{
    if (_nciHasSelection){
        _graph.frame = CGRectMake(0, labelHeight, self.bounds.size.width, self.bounds.size.height - labelHeight) ;//self.bounds;
    } else {
        _graph.frame = self.bounds;
    }
    [_graph layoutSubviews];
    if (_nciHasSelection){
        _selectedLabel.frame = CGRectMake(0, 0, self.bounds.size.width, labelHeight);;
        [self layoutSelectedPoint];
    }
}

- (void)addPoint:(double)arg val:(NSArray *)values{
    [self.chartData addObject:@[@(arg), values]];
}

- (NSArray *)getBoundaryValues{
    float minY = MAXFLOAT;
    float maxY = -MAXFLOAT;
    for (NSArray *points in self.chartData){
        for (NSNumber *point in points[1]){
            if ([point isKindOfClass:[NSNull class]]){
                continue;
            }
            float val = [point floatValue];
            if (val < minY){
                minY = val;
            }
            if (val > maxY){
                maxY = val;
            }
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
