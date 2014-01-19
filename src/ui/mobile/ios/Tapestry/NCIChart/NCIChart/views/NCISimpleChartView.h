//
//  NCISimpleChartView.h
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "NCIChartOptions.h"
#import "NCILine.h"

@class NCISimpleGraphView;

@interface NCISimpleChartView : UIView

@property (nonatomic, strong)NCISimpleGraphView *graph;
@property (nonatomic, strong)NSMutableArray *chartData;
@property (nonatomic, strong)UILabel *selectedLabel;

@property (nonatomic)bool nciUseDateFormatter;
@property (nonatomic)bool nciShowPoints;
@property (nonatomic)bool hasYLabels;
@property (nonatomic)bool nciIsFill;
@property (nonatomic)float nciLineWidth;
@property (nonatomic, strong)NSMutableArray* nciLineColors;

@property (nonatomic)bool nciHasSelection;
@property (nonatomic, strong)NSMutableArray* nciSelPointColors;
@property (nonatomic, strong)NSMutableArray* nciSelPointImages;
@property (nonatomic)NSArray* nciSelPointSizes;
@property (nonatomic, strong)UIFont* nciSelPointFont;

@property (nonatomic, strong)UIFont* nciXLabelsFont;
@property (nonatomic, strong)UIFont* nciYLabelsFont;
@property(nonatomic) float nciXLabelsDistance;
@property(nonatomic) float nciYLabelsDistance;

@property (nonatomic, copy) NSString* (^nciXLabelRenderer)(double);
@property (nonatomic, copy) NSString* (^nciYLabelRenderer)(double);
//callbacks
@property (nonatomic, copy) NSString* (^nciSelPointTextRenderer)(double, NSArray *);
@property (nonatomic, copy) void (^nciTapGridAction)(double, double);
//in persentage
@property (nonatomic)float topBottomGridSpace;

@property(nonatomic, strong)NCILine* nciBoundaryVertical;
@property(nonatomic, strong)NCILine* nciBoundaryHorizontal;
@property(nonatomic, strong)NCILine* nciGridVertical;
@property(nonatomic, strong)NCILine* nciGridHorizontal;
@property(nonatomic, strong)UIColor* nciGridColor;
@property(nonatomic)float nciGridLeftMargin;

-(id)initWithFrame:(CGRect)frame andOptions:(NSDictionary *)opts;

- (void)drawChart;
- (void)addSubviews;
- (void)addPoint:(double)arg val:(NSArray *)values;
- (NSArray *)getBoundaryValues;
- (void)layoutSelectedPoint;

@end
