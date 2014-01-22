### NCICharts: Linear charts and charts with range selectors for iOS

![alt text][Illustration]
[Illustration]: https://raw.github.com/FlowForwarding/tapestry/master/src/ui/mobile/ios/Tapestry/NCIChart/docs/default.png "NCI chart"

Simple

```ObjectiveC
#import "NCIChartView.h"

    NCISimpleChartView *chart = [[NCISimpleChartView alloc] initWithFrame:CGRectMake(0, 0, 400, 250)];
    [self.view addSubview:chart];
    
    int numOfPoints = 10;
    for (int ind = 0; ind < numOfPoints; ind ++){
        [chart addPoint:ind val:@[@(arc4random() % 5)]];
    }
``` 

Ranges

```ObjectiveC
#import "NCIChartView.h"

    NCIChartView *chart = [[NCIChartView alloc] initWithFrame:CGRectMake(0, 0, 300, 400)];
    [self.view addSubview:chart];
    int numOfPoints = 10;
    for (int ind = 0; ind < numOfPoints; ind ++){
        [chart addPoint:ind val:@[@(arc4random() % 5)]];
    }
```

Several lines

```ObjectiveC
#import "NCIChartView.h"
    
    NCISimpleChartView *chart = [[NCISimpleChartView alloc]
                                 initWithFrame:CGRectMake(30, 50, 400, 250)
                                 andOptions: @{nciIsFill: @(NO),
                                               nciSelPointSizes: @[@5, @10, @5]}];
                                               
```

Customization options

```ObjectiveC
    NCISimpleChartView *chart = [[NCISimpleChartView alloc]
                                 initWithFrame:CGRectMake(50, 30, 400, 250)
                                 andOptions: @{nciIsFill: @(NO),
                                               nciLineColors: @[[UIColor orangeColor], [NSNull null]],
                                               nciLineWidths: @[@2, [NSNull null]],
                                               nciHasSelection: @YES,
                                               nciSelPointColors: @[[UIColor redColor]],
                                               nciSelPointImages: @[[NSNull null], @"star"],
//                                               nciSelPointTextRenderer: ^(double argument, NSArray* values){
//        return [NSString stringWithFormat:@"Money:%.1f  for %.1f", value, argument];
//    },
                                               
                                               nciSelPointSizes: @[@10, [NSNull null]],
                                               nciSelPointFont: [UIFont fontWithName:@"MarkerFelt-Thin" size:12],
                                               nciSelPointFontColor : [UIColor redColor],
                                               nciXLabelsFont: [UIFont fontWithName:@"MarkerFelt-Thin" size:12],
                                               nciXLabelsColor: [UIColor blueColor],
                                               nciYLabelsFont: [UIFont fontWithName:@"MarkerFelt-Thin" size:12],
                                               nciYLabelsColor: [UIColor brownColor],
                                               nciXLabelsDistance: @150,
                                               nciYLabelsDistance: @30,
                                               nciYLabelRenderer: ^(double value){
        return [NSString stringWithFormat:@"%.1f$", value];
    },
//                                               nciTapGridAction: ^(double argument, double value, float xInGrid, float yInGrid){
//        
//    },
                                               nciShowPoints : @YES,
                                               nciUseDateFormatter: @YES,//nciXLabelRenderer
                                               nciBoundaryVertical: [[NCILine alloc] initWithWidth:1 color:[UIColor blackColor] andDashes:@[@2,@2]],
                                               nciBoundaryHorizontal: [[NCILine alloc] initWithWidth:2 color:[UIColor redColor] andDashes:nil],
                                               nciGridVertical: [[NCILine alloc] initWithWidth:1 color:[UIColor purpleColor] andDashes:nil],
                                               nciGridHorizontal: [[NCILine alloc] initWithWidth:2 color:[UIColor greenColor] andDashes:@[@2,@2]],
                                               nciGridColor: [[UIColor yellowColor] colorWithAlphaComponent:0.2],
                                               nciGridLeftMargin: @50,
                                               nciGridTopMargin: @50,
                                               nciGridBottomMargin: @40
                                               }];
    
    chart.backgroundColor = [[UIColor brownColor] colorWithAlphaComponent:0.2];
    [self.view addSubview:chart];
    
    int numOfPoints = 10;
    double dataPeriod = 1000*60*60*24*30;
    double step = dataPeriod/(numOfPoints - 1);
    for (int ind = 0; ind < numOfPoints; ind ++){
        //to use default date formatter for Y axis, provide arguments as  timeIntervalSince1970
        //and set nciXLabelRenderer option to YES
        [chart addPoint:[[[NSDate date] dateByAddingTimeInterval:- step *ind] timeIntervalSince1970] val:@[@(arc4random() % 5),
                                  @(arc4random() % 5)]];
    }            

``` 
