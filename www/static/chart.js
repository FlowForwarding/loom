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

NCI.initChart = function(date){
	NCI.chartData = [[ new Date(new Date(date)-1000*60*60*24).getTime(), 0],
		    [ new Date(date).getTime(), 0]];
     NCI.chart = new Dygraph(
		 document.getElementById("nciChart"),
		 NCI.chartData,
		 {
			 labels : ['NCI', 'NCI'],
			 dateWindow: [new Date(new Date(date)-1000*60*10).getTime(),  new Date(date).getTime()],
			 zoomCallback: function(minDate, maxDate, yRanges){
				 console.log(minDate);
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
