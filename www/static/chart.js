if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.chart = new google.visualization.BarChart(document.getElementById('visualization')); 
NCI.chartData = google.visualization.arrayToDataTable([
	['Hour', 'Index'],
	['2a.m',  1000],
	['3a.m',  1170],
	['4a.m',  660],
	['5a.m',  1030],
	['6a.m',  700],
	['7a.m',  1110],
	['9a.m',  1110],
	['7a.m',  1110]
]);  

NCI.drawChart = function() {
	NCI.chart.draw(NCI.chartData, NCI.chartOptions);
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
	  		easing: 'out'
	  	}
	};
   
   // data.addColumn('string', 'x');
   // data.addColumn('number', 'y');
   // data.addRow(['100', 123]);
   // data.addRow(['700', 17]);
  // var button = document.getElementById('b1');

   // button.onclick = function() {
   //   var newValue = 1000 - data.getValue(0, 1);
   //   data.setValue(0, 1, newValue);
   //   drawChart();
   // };
   NCI.drawChart();
}
