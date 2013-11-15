if (typeof NCI === 'undefined')
   NCI = {};   

// NCI.getChartHeight = function(){
//    	var chartHeight = 	$( window ).height()/2;
//        if ($( window ).width() < 400) 	{
//    	  chartWidth = 4*$( window ).width()/5; 
//    	  chartHeight = $( window ).height()/3;
//        };
//    	return chartHeight;
// };

NCI.chartData = [];

NCI.chart;

NCI.chartPeriods = {
	first: 0,
	minute: 1000*60,
	tenminutes: 1000*60*10,
	halfhour: 1000*60*30,
    halfday: 1000*60*60*12,
    day: 1000*60*60*24,
	twodays: 1000*60*60*24*2,
    fivedays: 1000*60*60*24*5,
	halfmnth: 1000*60*60*24*15,
    onemnth: 1000*60*60*24*30,
    threemnth: 1000*60*60*24*30*3,
    sixmnth: 1000*60*60*24*30*6,
    oneyear: 1000*60*60*24*30*12,
	threeyears: 1000*60*60*24*30*12*3,
    fiveyears: 1000*60*60*24*30*12*5,
	sixyears: 1000*60*60*24*30*12*6,
    tenyears: 1000*60*60*24*30*12*10
};

// day is first NCI.chartPeriods.day
NCI.upPeriods = [
	NCI.chartPeriods.day,
	NCI.chartPeriods.halfmnth, 
	NCI.chartPeriods.threemnth, 
	NCI.chartPeriods.oneyear, 
	NCI.chartPeriods.threeyear,
	NCI.chartPeriods.sixyears,
	NCI.chartPeriods.tenyears];

NCI.curChartPeriod = NCI.chartPeriods.day;
NCI.lastUpdateChartPeriod = NCI.chartPeriods.day;

NCI.initChart = function(date){
	NCI.chartData = [];
	NCI.chart = new Dygraph(
		 document.getElementById("nciChart"),
		 NCI.chartData,
		 {
			 labels : ['NCI', 'NCI'],
			 dateWindow: [new Date(new Date(date) - NCI.chartPeriods.halfhour).getTime(),  new Date(date).getTime()],
			 zoomCallback: function(minDate, maxDate, yRanges){
				 NCI.zoomLinks.removeClass('selected');
				 NCI.chart.updateDataset(minDate, maxDate, yRanges);		 
			 },
			 xValueFormatter: Dygraph.dateString_,
			 fillGraph: true,
			 axisLabelFontSize: 10,
			 xAxisLabelWidth: 60,
			 logscale: true,
			 yRangePad: 15,
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
 	 NCI.chart.updateDataset = function(minDate, maxDate, yRanges) {
		 //we detect do we need to add more time to chart, increaing is discret - NCI.upPeriods
		 var timeUp = false;
		 for (var index = 1; index < NCI.upPeriods.length; index++){	 
			 var period = NCI.upPeriods[index];
		 	 if (maxDate - minDate > NCI.curChartPeriod * (index*4 + 2)/27
				&& period > NCI.curChartPeriod){
					NCI.curChartPeriod = period;	
					timeUp = true; 	
				};
		 };
		 
		 if (timeUp) {
			console.log("up");
			NCI.chartData =  [[new Date(new Date() - NCI.time_adjustment - NCI.curChartPeriod).getTime(), 0]].concat(NCI.chartData);
			NCI.chart.updateOptions({
				file: NCI.chartData
			});
			NCI.Connection.moreData(new Date() - NCI.curChartPeriod - NCI.time_adjustment, 
				new Date() - NCI.time_adjustment, NCI.numOfPoints);
			NCI.lastUpdateChartPeriod  = NCI.curChartPeriod;
			return;
		 };
		
		
		//we detect do we need to remove time from chart period, decreasing is discret - NCI.upPeriods
		for(var k = NCI.upPeriods.length -2; k > -1; k--){
			var period = NCI.upPeriods[k];
			if (maxDate - minDate < NCI.curChartPeriod * (k*4 + 2)/27
				&& period < NCI.curChartPeriod){
					console.log("down");
					NCI.curChartPeriod =  period;
					var newChartData = [];
					$.each(NCI.chartData, function(ind, val){
						if (val[0] > ( NCI.lastUpdateTimeVal - NCI.curChartPeriod)){
							newChartData.push(val);
						};
					});
					NCI.chartData = newChartData;
					NCI.chart.updateOptions({
						file: NCI.chartData
					});
					NCI.Connection.moreData(new Date() - NCI.curChartPeriod - NCI.time_adjustment, 
						new Date() - NCI.time_adjustment, NCI.numOfPoints);
					NCI.lastUpdateChartPeriod  = NCI.curChartPeriod;
					return;
			  };
		};	
	};
};
