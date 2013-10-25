if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.chart = new google.visualization.LineChart(document.getElementById('visualization')); 
NCI.chartData; 

NCI.drawChart = function() {
	NCI.chart.draw(NCI.chartData, NCI.chartOptions);
};
//
NCI.addValueToChart = function(params) {
	if (!NCI.chartData) {
		NCI.chartData = google.visualization.arrayToDataTable([
			['Time', 'NCI'],
			[params.time, params.NCI]
		]);
	} else {
		NCI.chartData.insertRows(NCI.chartData.J.length,
			[[params.time.toString(), params.NCI]]);
	};
	
	if (NCI.chartData.J.length > 59)
	    NCI.chartData.removeRow(0);	
	
	NCI.drawChart();
};

NCI.getChartHeight = function(){
	var chartHeight = 	$( window ).height()/2;
    if ($( window ).width() < 400) 	{
	  chartWidth = 4*$( window ).width()/5; 
	  chartHeight = $( window ).height()/3;
    };
	return chartHeight;
};

NCI.initChart = function(){
	var chartWidth = $( window ).width();
	if (chartWidth > 1000)
		chartWidth = 1000;
	
	NCI.chartOptions = {
	  	width: chartWidth,
	  	height: NCI.getChartHeight(),
	  	legend: "none",
	  	title: '',
		// vAxis: {title: 'Year',  titleTextStyle: {color: 'red'}},
		animation: {
			duration: 1000,
			easing: 'linear'
		}
	};
};
