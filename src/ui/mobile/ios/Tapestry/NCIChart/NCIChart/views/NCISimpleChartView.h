//
//  NCISimpleChartView.h
//  NCIChart
//
//  Created by Ira on 12/20/13.
//  Copyright (c) 2013 FlowForwarding.Org. All rights reserved.
//

#import <UIKit/UIKit.h>
#import "NCIChartOptions.h"

@class NCISimpleGraphView;

@interface NCISimpleChartView : UIView

@property (nonatomic, strong)NCISimpleGraphView *graph;
@property (nonatomic, strong)NSMutableArray *chartData;
@property (nonatomic, strong)UILabel *selectedLabel;

@property (nonatomic)bool hasYLabels;
@property (nonatomic)bool nciIsFill;
@property (nonatomic)float nciLineWidth;
@property (nonatomic, strong)UIColor* nciLineColor;

@property (nonatomic)bool nciHasSelection;
@property (nonatomic, strong)UIColor* nciSelPointColor;
@property (nonatomic, strong)NSString* nciSelPointImage;
@property (nonatomic)float nciSelPointSize;
@property (nonatomic, strong)UIFont* nciSelPointFont;

@property (nonatomic, strong)UIFont* nciXLabelsFont;
@property (nonatomic, strong)UIFont* nciYLabelsFont;
@property(nonatomic) float nciXLabelsDistance;
@property(nonatomic) float nciYLabelsDistance;

@property (nonatomic, copy) NSString* (^nciXLabelRenderer)(double);
@property (nonatomic, copy) NSString* (^nciYLabelRenderer)(double);
//callbacks
@property (nonatomic, copy) NSString* (^nciSelPointTextRenderer)(double, double);
@property (nonatomic, copy) void (^nciTapGridAction)(double, double);

//in persentage
@property (nonatomic)float topBottomGridSpace;

-(id)initWithFrame:(CGRect)frame andOptions:(NSDictionary *)opts;

- (void)addSubviews;
- (void)addPoint:(double)arg val:(NSNumber *)value;
- (NSArray *)getBoundaryValues;
- (void)layoutSelectedPoint;

@end
