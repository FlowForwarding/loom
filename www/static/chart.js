if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.chart = new google.visualization.LineChart(document.getElementById('visualization')); 
NCI.chartData; 

NCI.drawChart = function() {
	NCI.chart.draw(NCI.chartData, NCI.chartOptions);
};
//
NCI.addValueToChart = function(params) {
	if (!NCI.chartData)
		NCI.chartData = google.visualization.arrayToDataTable([
			['Time', 'NCI', 'QPS', 'NEP'],
			[params.time, 0, 0, 0]
		]);
	
	if (NCI.chartData.J.length > 10)
	    NCI.chartData.removeRow(0);
	
	var isNew = true;
	var insertInd;
	$.each(NCI.chartData.J, function(ind, el){
		if (el.c[0].v == params.time.toString()){
			isNew = false;
			insertInd = ind;
			return false;
		};
	});
	
	if (!isNew){ 
		if (params.NCI)
			NCI.chartData.setValue(insertInd, 1, params.NCI);
		if (params.QPS)
			NCI.chartData.setValue(insertInd, 2, params.QPS);
		if (params.NEP)
			NCI.chartData.setValue(insertInd, 3, params.NEP);
	} else {
		NCI.chartData.insertRows(NCI.chartData.J.length,
			[[params.time.toString(), params.NCI, params.QPS, params.NEP]]);
	};
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
	  	title: ''//,
		// vAxis: {title: 'Year',  titleTextStyle: {color: 'red'}},
		// animation: {
		// 	  		duration: 1000,
		// 	  		easing: 'linear'
		// 	  	}
	};
};
