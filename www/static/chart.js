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

NCI.jqPlotDateFormatter = function(f, date) {
	console.log();
	if (date == 0 || date == 1)
		return "";
	var show = true;	
	if (NCI.chartData.length > 40){
		show = xaxiscounter > 2;
	} else if (NCI.chartData.length > 15){
		show = xaxiscounter % 2 == 0;
	};
	xaxiscounter = xaxiscounter > 2 ? 0 : xaxiscounter + 1;
	if (!show)
		return " ";
	var dimention = NCI.slider.xAxesScale[NCI.slider[0].value].indexDim;
	return NCI.parceDateWithDimention(date, dimention)
};

NCI.chart = $.jqplot ('visualization', [[[0,0]]], {
    axesDefaults: {
       tickRenderer: $.jqplot.CanvasAxisTickRenderer ,
       tickOptions: {
          angle: -30,
          fontSize: '10pt',
		  showGridline: true
	   }
    },
	axes: {
	      xaxis: {
	        renderer: $.jqplot.CategoryAxisRenderer,
	        tickOptions: {
				formatter: NCI.jqPlotDateFormatter
			}
	      },
		  yaxis: {
			  label:'NCI',
			  labelOptions:{
				  fontFamily:'Helvetica',
				  fontSize: '14pt'
			  }
		  }
	 },
	 seriesDefaults: {
	 	showMarker: false,
		color: "#205BBB",
		shadow: false,
		lineWidth: 1
	 },
	 grid :{
		 drawGridLines: false,
		 background: '#ffffff',  
		 drawBorder: false,
		 shadow: false
	 },
	 highlighter: {
		 show: true,
		 tooltipOffset: 4,
		 tooltipAxes: 'y'
	 }
});

NCI.chartData = [];

NCI.addValueToChart = function(params) {
	var newData = NCI.chartData;
	newData.push([params.time.toString(), params.NCI]);
    NCI.chart.series[0].data = newData;
	if (newData.length > NCI.slider.xAxesScale[NCI.slider[0].value].pointsNum)
		newData.shift();
	NCI.chart.resetAxesScale();
    NCI.chart.replot( {data: [newData.slice(0)]});
};
