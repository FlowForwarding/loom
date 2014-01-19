### NCICharts: Linear charts and charts with range selectors for iOS

![alt text][Illustration]
[Illustration]: https://raw.github.com/FlowForwarding/tapestry/master/src/ui/mobile/ios/Tapestry/NCIChart/docs/default.png "NCI chart"


```ObjectiveC
NCIChartView *chart = [[NCIChartView alloc] initWithFrame:CGRectMake(0, 0, 300, 400)];
[self.view addSubview:chart];
int numOfPoints = 10;
for (int ind = 0; ind < numOfPoints; ind ++){
  [chart addPoint:ind val:@[@(arc4random() % 5)]];
}
