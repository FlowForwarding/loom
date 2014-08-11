NCI.nciHistogram = (function(){
	var me = $('#nciHistogram');
	
	var internalEndpointsCheckbox = $('#histogramDetailsInternal');
	var barWidth = 4;
	var chart = d3.select("#nciHistogram");
	var chartDetails = d3.select("#nciHistogramDetails");
	var margin = {top: 10, right: 60, bottom: 40, left:40},
	    width = 600,
	    height = 250;
	var color = d3.scale.category10();
	var notNetworkColor = "#fff";		
	var detailsGraphForce;	
	var force;
	
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
	
	var tooltip;
    var activityDetails;
	var detailsDim;
	var detailsHeight;
	me.showDetails = function(d){
		if  (!activityDetails) {
			detailsWidth = $(window).width();
			detailsHeight = $(window).height();
		    activityDetails = chartDetails.append('svg')
				.attr('width', detailsWidth)
			    .attr('height', detailsHeight);
				
			tooltip = chartDetails.append("div").attr("class", "endpoint-tooltip");		
		} else {
			activityDetails.text("");
		};
		$('.histogram-details-graph').show();
			
		if (d.Endpoints.length > NCI.max_vertices)
		    return;	
	    var graph = new NCI.graphBuilder([d]).graph;
		
		//max 120 for 0, min 10 for NCI.max_vertices
		var linkDistance = 5 + 160 - Math.floor( 160 * d.Endpoints.length/NCI.max_vertices)
		var charge = -10 - (60 - Math.floor( 60 * d.Endpoints.length/NCI.max_vertices))
		
		var verticlesLimitForRadius = 250
		var radius = 4
		if (d.Endpoints.length < verticlesLimitForRadius) {
			radius = 3 + 2 - Math.floor( 2*d.Endpoints.length/verticlesLimitForRadius);
		}
	//
		force = d3.layout.force()
			.charge(charge)
			.linkDistance(linkDistance)
			.size([detailsWidth, detailsHeight])
			.linkStrength(1).nodes(graph.nodes).links(graph.links).start();
			
		var link = activityDetails.selectAll(".link")
		    .data(graph.links)
			.enter().append("line")
			.attr("class", "activities_link");  	
				  
		chartDetails.node = activityDetails.selectAll(".node")
		    .data(graph.nodes)
		    .enter().append("circle")
			.call(force.drag)
		    .attr("r", radius)
			.style("fill", function(d) {
				return me.colorifyEndpoint(internalEndpointsCheckbox[0].checked, d);
			 }).on('mouseover', function(d){
	 			var info = d.fullname;
	 			if (d.external){
	 				info += "<br>doesn't belong to activity";
	 			};
	 			info += "<br>" + d.connections + " connections";
				tooltip.html(info).style("top", d.py - 10).
							       style("left", d.px + 10).style("display", "inline");
           })
		   .on('mouseout', function(){
			   tooltip.style("display", "none");
		   });

		force.on("tick", function() { 
			link.attr("x1", function(d) { return d.source.x; })
			    .attr("y1", function(d) { return d.source.y; })
				.attr("x2", function(d) { return d.target.x; })
				.attr("y2", function(d) { return d.target.y; });
				
			chartDetails.node.attr("cx", function(d) { return d.x; })
			    .attr("cy", function(d) { return d.y; });
		});
	};
	
	me.colorifyEndpoint = function(devided, endpoint){
		if (endpoint.external)
		    return "#ff0000";
		if (devided && NCI.isExternal(endpoint.name)){
			return notNetworkColor;
		}
		return color(0);
	};
	
	me.clean = function(){
		d3.select("#nciHistogramDetails svg").text("");
		chart.text("");
	};
	
	internalEndpointsCheckbox.on('click', function(event){
		var checked = this.checked;
		chartDetails.node.style("fill", function(d) {
			return me.colorifyEndpoint(checked, d);
		});
	});
	
	return me;
}());

$('.histogramDetailsClose').on('click', function(){
	NCI.socialGraph.graph = undefined;
	$('.histogram-details-graph').hide();
});
