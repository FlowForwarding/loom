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
	threehours: 1000*60*60*3,
	sixhours: 1000*60*60*6,
    halfday: 1000*60*60*12,
    day: 1000*60*60*24,
	twodays: 1000*60*60*24*2,
    fivedays: 1000*60*60*24*5,
	sixdays: 1000*60*60*24*6,
	tendays: 1000*60*60*24*10,
	halfmnth: 1000*60*60*24*15,
    onemnth: 1000*60*60*24*30,
    threemnth: 1000*60*60*24*30*3,
    sixmnth: 1000*60*60*24*30*6,
	eightmnth: 1000*60*60*24*30*8,
	tenmnth: 1000*60*60*24*30*10,
    oneyear: 1000*60*60*24*30*12,
	threeyears: 1000*60*60*24*30*12*3,
    fiveyears: 1000*60*60*24*30*12*5,
	sixyears: 1000*60*60*24*30*12*6,
    tenyears: 1000*60*60*24*30*12*10
};

NCI.curChartPeriod = NCI.chartPeriods.halfmnth;
NCI.detailedChartPeriod = NCI.chartPeriods.sixhours;
NCI.rangeStartDate = $("#range-start-date");
NCI.rangeMiddleDate = $("#range-middle-date");

NCI.months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

NCI.dateForFirstRangePeriod = function(date){
	return date.getDate() + " " + NCI.months[date.getMonth()] + " " + date.getFullYear();
};

NCI.dateForSecondRangePeriod = function(date){
	return NCI.months[date.getMonth()] + " " + date.getFullYear();
};

NCI.dateForThirdRangePeriod = function(date){
	return date.getFullYear();
};



NCI.initChart = function(date){
	NCI.chartData = [];
	NCI.rangeStartDate.text(NCI.dateForFirstRangePeriod(new Date(new Date() - NCI.curChartPeriod)));
	NCI.rangeMiddleDate.text(NCI.dateForFirstRangePeriod(new Date(new Date() - NCI.curChartPeriod/2)));
	
	NCI.chart = new Dygraph(
		 document.getElementById("nciChart"),
		 NCI.chartData,
		 {
			 labels : ['NCI', 'NCI'],
			 dateWindow: [(new Date(new Date(date) - NCI.detailedChartPeriod)).getTime(),  new Date(date).getTime()],
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
		 //if it is first period for not scaled chart ( half month ) 
		 //and range becomes more then 10 days we make jump
		 //to increase range chart period to 1 year
		 var timeUp = false;
		 if (NCI.curChartPeriod == NCI.chartPeriods.halfmnth && (maxDate - minDate) > NCI.chartPeriods.sixdays){
			 NCI.curChartPeriod = NCI.chartPeriods.oneyear;
			 NCI.rangeStartDate.text(NCI.dateForSecondRangePeriod(new Date(new Date() - NCI.curChartPeriod - NCI.time_adjustment)));
			 NCI.rangeMiddleDate.text(NCI.dateForSecondRangePeriod(new Date(new Date() - NCI.curChartPeriod/2 - NCI.time_adjustment)));
			 timeUp = true;
		 //if it is second period for not scaled chart ( one year ) 
		 //and range becomes more then 8 month we make jump
		 //to increase range chart period to 10 years
		 } else  if (NCI.curChartPeriod == NCI.chartPeriods.oneyear && (maxDate - minDate) > NCI.chartPeriods.tenmnth) {
			 NCI.curChartPeriod = NCI.chartPeriods.tenyears;
			 NCI.rangeStartDate.text(NCI.dateForThirdRangePeriod(new Date(new Date() - NCI.curChartPeriod - NCI.time_adjustment)));
			 NCI.rangeMiddleDate.text(NCI.dateForThirdRangePeriod(new Date(new Date() - NCI.curChartPeriod/2 - NCI.time_adjustment)));
			 timeUp = true;
		 };
		 
		 if (timeUp) {
			console.log("up");
			NCI.chartData =  [[new Date(new Date() - NCI.time_adjustment - NCI.curChartPeriod).getTime(), 0]].concat(NCI.chartData);
			NCI.chart.updateOptions({
				file: NCI.chartData
			});
			
			NCI.Connection.moreData(new Date() - NCI.curChartPeriod - NCI.time_adjustment, 
				new Date() - NCI.time_adjustment, NCI.numOfPoints);
			return;
		 };
		 
		 var timeDown = false;
		 //if it is second period for not scaled chart ( one year ) 
		 //and range becomes less then 6 days we make jump
		 //to decrease range chart period to half month
		 if (NCI.curChartPeriod == NCI.chartPeriods.oneyear && (maxDate - minDate) < NCI.chartPeriods.sixdays){
			 NCI.curChartPeriod = NCI.chartPeriods.halfmnth;
			 NCI.rangeStartDate.text(NCI.dateForFirstRangePeriod(new Date(new Date() - NCI.curChartPeriod - NCI.time_adjustment)));
			 NCI.rangeMiddleDate.text(NCI.dateForFirstRangePeriod(new Date(new Date() - NCI.curChartPeriod/2 - NCI.time_adjustment)));
			 timeDown = true;
		//if it is thirdperiod for not scaled chart ( 10 years ) 
		//and range becomes less then 5 years we make jump
		//to decrease range chart period to one year			 
		 } else if (NCI.curChartPeriod == NCI.chartPeriods.tenyears && (maxDate - minDate) < NCI.chartPeriods.fiveyears) {
			 NCI.curChartPeriod = NCI.chartPeriods.oneyear;
			 NCI.rangeStartDate.text(NCI.dateForSecondRangePeriod(new Date(new Date() - NCI.curChartPeriod - NCI.time_adjustment)));
			 NCI.rangeMiddleDate.text(NCI.dateForSecondRangePeriod(new Date(new Date() - NCI.curChartPeriod/2 - NCI.time_adjustment)));
		 	 timeDown = true;
		 };
		 
		if (timeDown){
			console.log("down");
			var newChartData = [];
			$.each(NCI.chartData, function(ind, val){
				if (val[0] > ( new Date() - NCI.time_adjustment - NCI.curChartPeriod)){
					newChartData.push(val);
				};
			});
			NCI.chartData = newChartData;
			NCI.chart.updateOptions({
				file: NCI.chartData
			});
			NCI.Connection.moreData(new Date() - NCI.curChartPeriod - NCI.time_adjustment, 
				new Date() - NCI.time_adjustment, NCI.numOfPoints);
			return;
		};	
	};
};
