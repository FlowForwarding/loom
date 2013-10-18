if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.chart = new google.visualization.LineChart(document.getElementById('visualization')); 
NCI.chartData = google.visualization.arrayToDataTable([
	['Time', 'NCI'],
	[new Date().toString(), 0]
]);  

NCI.drawChart = function() {
	console.log(NCI.chartData);
	NCI.chart.draw(NCI.chartData, NCI.chartOptions);
};
//
NCI.addValueToChart = function(params) {
	if (NCI.chartData.J.lenght > 10)
	    NCI.chartData.removeRow(0);
	NCI.chartData.insertRows(NCI.chartData.J.length, [[params.time.toString(), params.NCI]]);
	NCI.drawChart();
};

function initChart (){
	var chartWidth = 3 *$( window ).width()/4;
	if (chartWidth > 700)
		chartWidth = 700;
    if ($( window ).width() < 400) 	
	  chartWidth = 3*$( window ).width()/5; 
	
	
	NCI.chartOptions = {
	  	width: chartWidth,
	  	height: $( window ).height()/2,
	  	legend: "none",
	  	title: '',
		// vAxis: {title: 'Year',  titleTextStyle: {color: 'red'}},
		animation: {
	  		duration: 1000,
	  		easing: 'in'
	  	}
	};
   NCI.drawChart();
};
