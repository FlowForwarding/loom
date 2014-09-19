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
	twoyears: 1000*60*60*24*30*12*2,
	threeyears: 1000*60*60*24*30*12*3,
    fiveyears: 1000*60*60*24*30*12*5,
	sixyears: 1000*60*60*24*30*12*6,
    tenyears: 1000*60*60*24*30*12*10
};

NCI.rangeStartDate = $("#range-start-date");
NCI.rangeMiddleDate = $("#range-middle-date");

NCI.months = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

NCI.setBottomChartDates = function(period){
	if (period < NCI.chartPeriods.sixmnth){
		NCI.rangeStartDate.text(NCI.dateForFirstRangePeriod(period));
		NCI.rangeMiddleDate.text(NCI.dateForFirstRangePeriod(period/2));
	} else if (period < NCI.chartPeriods.threeyears){
		NCI.rangeStartDate.text(NCI.dateForSecondRangePeriod(period));
		NCI.rangeMiddleDate.text(NCI.dateForSecondRangePeriod(period/2));
	} else {
		NCI.rangeStartDate.text(NCI.dateForThirdRangePeriod(period));
		NCI.rangeMiddleDate.text(NCI.dateForThirdRangePeriod(period/2));
	};

};

NCI.dateForFirstRangePeriod = function(period){
	var date = new Date(new Date() - period - NCI.time_adjustment);
	return date.getDate() + " " + NCI.months[date.getMonth()] + " " + date.getFullYear();
};

NCI.dateForSecondRangePeriod = function(period){
	var date = new Date(new Date() - period - NCI.time_adjustment);
	return NCI.months[date.getMonth()] + " " + date.getFullYear();
};

NCI.dateForThirdRangePeriod = function(period){
	var date = new Date(new Date() - period - NCI.time_adjustment);
	return date.getFullYear();
};

NCI.dateString_ = function(date) {
	var d = new Date(date);
	
	// Get the year:
	var year = "" + d.getFullYear();
	// Get a 0 padded month string
	var month = NCI.months[d.getMonth()];  //months are 0-offset, sigh
	// Get a 0 padded day string
	var day = Dygraph.zeropad(d.getDate());
	
	var ret = "";
	var frac = d.getHours() * 3600 + d.getMinutes() * 60 + d.getSeconds();
	if (frac) ret = " " + Dygraph.hmsString_(date);
	
	return year + "-" + month + "-" + day + ret;
};

NCI.initChart = function(date){
	NCI.chartData = [];
	NCI.chart = new Dygraph(
		 document.getElementById("nciChart"),
		 NCI.chartData,
		 {
			 labels : ['NCI', 'NCI'],
			 zoomCallback: function(minDate, maxDate, yRanges){
				 NCI.zoomLinks.removeClass('selected');	 
			 },
			 fillGraph: true,
			 connectSeparatedPoints: true,
			 yRangePad: 10,
			 gridLineWidth: 0.1,
			 axisLabelFontSize: 10,
			 xAxisLabelWidth: 70,
			 logscale: true,
			 axes : { x : 
				 {
					 axisLabelFormatter: NCI.dateString_, 
					 valueFormatter: NCI.dateString_,
					 ticker : Dygraph.dateTicker,
					 pixelsPerLabel: 100
				 } 
			 },
			 ylabel: 'NCI',
			// legend: 'always',
			 labelsDivStyles: { 'textAlign': 'right' },
			 showRangeSelector: true
		 }
	 );	
};
