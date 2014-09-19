NCI.nciHistogram = (function(){
	var me = $('#nciHistogram');
	
	//var internalEndpointsCheckbox = $('#histogramDetailsInternal');
	var barWidth = 4;
	var chart = d3.select("#nciHistogram");
	var margin = {top: 10, right: 60, bottom: 40, left:40},
	    width = 600,
	    height = 250;

	me.show = function(){
		chart.text("");
		var endpointsMax = d3.max(NCI.Communities, function(d) { return d.Size; });
		var endpointsMin;
		var endpointsScale;
		if (endpointsMax > 100) {
			endpointsMin = 0.5;
			endpointsScale = d3.scale.log();
		} else {
			endpointsMin = 0;
			endpointsScale = d3.scale.linear();
		}
		endpointsScale.domain([endpointsMax, endpointsMin])
				.range([0, height - margin.top - margin.bottom]);
		
		var activitiesScale = d3.scale.linear();
		var activitiesMin = 0;
		if (NCI.Communities.length < 800){
			activitiesScale = d3.scale.linear();
			activitiesMin = 0;
		} else{
			activitiesScale = d3.scale.log();
			activitiesMin = 0.5;
		};
		activitiesScale.domain([NCI.Communities.length + 1, activitiesMin])
					    .range([width - margin.right - margin.left, margin.left]);

		var activitiesAxis = d3.svg.axis()
		    .scale(activitiesScale)
		    .orient('bottom')
		    .tickSize(0)
			.ticks(Math.min(NCI.Communities.length + 2, 10),  d3.format("d"))
		    .tickPadding(8);

		var endpointsAxis = d3.svg.axis()
		    .scale(endpointsScale)
		    .orient('left')
			.tickSize(0)
			.ticks(10,  d3.format("d"))
		    .tickPadding(10);

		var barChartSvg = chart.append('svg')
		    .attr('width', width)
		    .attr('height', height)
		    .append('g')
		    .attr('transform', 'translate(' + margin.left + ', ' + margin.top + ')');

        //draw bars
        var index = 0;
		barChartSvg.selectAll('g')
		    .data(NCI.Communities)
		    .enter().append('rect')
		    .attr('x', function(d) { return activitiesScale(NCI.Communities.length - index++) - barWidth/2})
		    .attr('y', function(d) { return endpointsScale(d.Size)}) //- selfwidth
		    .attr('width', function(d) { return barWidth})
		    .attr('height',function(d) { return height - margin.top - margin.bottom - endpointsScale(d.Size) })
			.on("click", function(d) { 
				if (NCI.nciHistogram.selectedBar)
				    NCI.nciHistogram.selectedBar.setAttribute("fill", "black");
				NCI.nciHistogram.selectedBar = this; 
				this.setAttribute("fill", "rgb(31, 119, 180)");
				me.showDetails(d);
			 });

		barChartSvg.append("circle").attr("cy", endpointsScale(NCI.timestampNCI))
		    .attr("cx", activitiesScale(NCI.timestampNCI) ).style("fill", "red").attr("r", 6);	
	    barChartSvg.append("circle").attr("cy", endpointsScale(endpointsMin))
		    .attr("cx", activitiesScale(NCI.timestampNCI) ).style("fill", "red").attr("r", 4);
		barChartSvg.append("circle").attr("cy", endpointsScale(NCI.timestampNCI))
		    .attr("cx", activitiesScale(activitiesMin) ).style("fill", "red").attr("r", 4);
		//draw axis 
		barChartSvg.append('g')
		    .attr('class', 'x axis')
			.attr('transform', 'translate(0,' + (height - margin.top - margin.bottom) + ')')
			.call(activitiesAxis);
		barChartSvg.append('g')
			.attr('class', 'y axis')
			.attr('transform', 'translate(' + margin.left + ')')
			.call(endpointsAxis);	
		//draw axis labels																					
		barChartSvg.append('text').
		attr('style', 'font-weight:bold').
		html('Activities Sorted by Size&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;j').attr('x', width/2 - 100).attr('y', height - margin.top - 5);
		barChartSvg.append('text').
		attr('style', 'font-weight:bold').
		html('Number of Endpoints per Activity&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;X[j]').attr('x', -height/2 - 70).attr('y', -10)
		.attr('transform', 'rotate(-90)');

	};
	
	me.showDetails = function(d){
		$('.histogram-details-graph').show();
		var color = d3.scale.category10();
		me.socialGraph = new NCI.socialGraph(".histogram-details-graph", 
		   {
			   graphBuilder: new NCI.graphBuilder([d]),
			   numOfPoints: d.Endpoints.length,
			   width: $(window).width(), 
			   height: $(window).height(),
			   notNetworkColor: "#fff",
			   isClustered: true,
			legendData : [[color(0), "endpoint in activity"],
			    ["red", "endpoint in a different activity"],
			    ["#FFFFFf", "external endpoint"]],
			   radius: function(){
			    	var verticlesLimitForRadius = 250
			   	 	var radius = 4
			   	 	if (d.Endpoints.length < verticlesLimitForRadius) {
			   	 		radius = 3 + 2 - 2*d.Endpoints.length/verticlesLimitForRadius;
			   	 	}
					return radius;
			   },
			   linkDistance: function(){
				   return (5 + 160 * (NCI.max_vertices - d.Endpoints.length)/NCI.max_vertices);
			   }
		   });
        me.socialGraph.show(true);
	};

	me.clean = function(){
		chart.text("");
		if (me.socialGraph)
		    me.socialGraph.clean();
	};
	
	return me;
}());

$('.histogramDetailsClose').on('click', function(){
	NCI.nciHistogram.socialGraph.clean()
    $('.histogram-details-graph').hide();
 });