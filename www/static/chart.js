function initChart (){
   var options = {
	   width: 3*$( window ).width()/4,
	   height: $( window ).height()/2,
	   legend: "none",
       title: '',
      // vAxis: {title: 'Year',  titleTextStyle: {color: 'red'}},
	   animation: {
		   duration: 1000,
		   easing: 'out'
	   }
   };
   
   var data = google.visualization.arrayToDataTable([
     ['Hour', 'Index'],
     ['2a.m',  1000],
     ['3a.m',  1170],
     ['4a.m',  660],
     ['5a.m',  1030],
	 ['6a.m',  700],
	 ['7a.m',  1110],
	 ['7a.m',  1110],
	 ['7a.m',  1110],
   ]);
   
   var chart = new google.visualization.BarChart(document.getElementById('visualization'));
   
   // data.addColumn('string', 'x');
   // data.addColumn('number', 'y');
   // data.addRow(['100', 123]);
   // data.addRow(['700', 17]);
   var button = document.getElementById('b1');
   function drawChart() {
	   chart.draw(data, options);
   };
   // button.onclick = function() {
   //   var newValue = 1000 - data.getValue(0, 1);
   //   data.setValue(0, 1, newValue);
   //   drawChart();
   // };
   drawChart();
}
