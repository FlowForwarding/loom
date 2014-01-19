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
    
[self.view addSubview:chart];
int numOfPoints = 10;
for (int ind = 0; ind < numOfPoints; ind ++){
    [chart addPoint:ind val:@[@(arc4random() % 5),
                              @(arc4random() % 5),
                              @(arc4random() % 5)]];
}

``` 
