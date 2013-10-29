if (typeof NCI === 'undefined')
   NCI = {};   

NCI.getChartHeight = function(){
   	var chartHeight = 	$( window ).height()/2;
       if ($( window ).width() < 400) 	{
   	  chartWidth = 4*$( window ).width()/5; 
   	  chartHeight = $( window ).height()/3;
       };
   	return chartHeight;
};

$('#visualization').height(NCI.getChartHeight());

var xaxiscounter = 0;

NCI.chartData = [];

NCI.chart;

NCI.curChartPeriod = 0;

NCI.chartPeriods = {
	first: 0,
    halfday: 1000*60*60*12,
    day: 1000*60*60*24,
	twodays: 1000*60*60*24*2,
    fivedays: 1000*60*60*24*5,
    onemnth: 1000*60*60*24*30,
    threemnth: 1000*60*60*24*30*3,
    sixmnth: 1000*60*60*24*30*6,
    oneyear: 1000*60*60*24*30*12,
    fiveyears: 1000*60*60*24*30*12*5,
    tenyears: 1000*60*60*24*30*12*10
};

NCI.initChart = function(date){
	NCI.chartData = [[ new Date(new Date(date)-NCI.chartPeriods.day).getTime(), 0],
		    [ new Date(date).getTime(), 0]];
     NCI.chart = new Dygraph(
		 document.getElementById("nciChart"),
		 NCI.chartData,
		 {
			 labels : ['NCI', 'NCI'],
			 dateWindow: [new Date(new Date(date)-1000*60*10).getTime(),  new Date(date).getTime()],
			 zoomCallback: function(minDate, maxDate, yRanges){
 				 if (NCI.lastUpdateTimeVal.getTime() - minDate > NCI.chartPeriods.halfday *3/2 
				 && NCI.chartPeriods.fivedays > NCI.curChartPeriod  ){
					 NCI.curChartPeriod = NCI.chartPeriods.fivedays;
					 NCI.chartData =  [[new Date(new Date() - NCI.time_adjustment - NCI.curChartPeriod).getTime(), 0]].concat(NCI.chartData),
					 NCI.chart.updateOptions({
						 file: NCI.chartData
					 });
				 } else {
					 if (NCI.lastUpdateTimeVal.getTime() - minDate< NCI.chartPeriods.halfday 
					 	&& NCI.chartPeriods.fivedays<= NCI.curChartPeriod ){
							NCI.curChartPeriod = NCI.chartPeriods.day;
							var newChartData = [];
							$.each(NCI.chartData, function(ind, val){
								if (val[0] > ( NCI.lastUpdateTimeVal - NCI.chartPeriods.day)){
									newChartData.push(val);
								};
							});
							NCI.chartData = newChartData;
							NCI.chart.updateOptions({
								file: NCI.chartData
							});
						};
					}	 
			 },
			 xValueFormatter: Dygraph.dateString_,
			 axisLabelFontSize: 10,
			 xAxisLabelWidth: 60,
			 logscale: true,
			 axes : { x : 
				 {
					 axisLabelFormatter: Dygraph.dateString_, 
					 ticker : Dygraph.dateTicker 
				 } 
			 },
			 ylabel: 'NCI',
			// legend: 'always',
			 labelsDivStyles: { 'textAlign': 'right' },
			 showRangeSelector: true
          }
	  );		
};

NCI.addValueToChart = function(params) {
	var newData = NCI.chartData;
	newData.push([params.time.toString(), params.NCI]);
    NCI.chart.series[0].data = newData;
	if (newData.length > NCI.slider.xAxesScale[NCI.slider[0].value].pointsNum)
		newData.shift();
	NCI.chart.resetAxesScale();
    NCI.chart.replot( {data: [newData.slice(0)]});
};
