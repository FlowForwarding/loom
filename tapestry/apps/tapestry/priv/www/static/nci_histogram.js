NCI.nciHistogram = (function(){
	var me = $('#nciHistogram');
	
	var barWidth = 4;
	var chart = d3.select("#nciHistogram");
	var margin = {top: 40, right: 60, bottom: 40, left:40},
	    width = 600,
	    height = 350;
	
	me.show = function(){
		chart.text("");
		var endpointsMax = d3.max(NCI.Communities, function(d) { return d.Size; });
		var endpointsMin;
		var endpointsScale;
		if (endpointsMax > 100) {
			endpointsMin = 1;
			endpointsScale = d3.scale.log();
		} else {
			endpointsMin = 0;
			endpointsScale = d3.scale.linear();
		}
		endpointsScale.domain([endpointsMax, endpointsMin])
				.range([0, height - margin.top - margin.bottom]);
		
		var activitiesScale = d3.scale.linear();
		var activitiesMin = 0;
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
			.on("click", me.showDetails);

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
		html('Activities Sorted by Size&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;j').attr('x', width/2 - 100).attr('y', height - 45);
		barChartSvg.append('text').
		attr('style', 'font-weight:bold').
		html('Number of Endpoints per Activity&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;X[j]').attr('x', -height/2 - 70).attr('y', -10)
		.attr('transform', 'rotate(-90)');

	};
	
	me.showDetails = function(d){
		chart.select("#bar_endpoints").remove();
		if (d.Size > 300)
		return;
		var detailsDim = 350;
		var activityDetails = chart.append('svg')
			.attr("id","bar_endpoints")
			.attr('width', detailsDim)
		    .attr('height', detailsDim);
			
	    var graph = NCI.prepareDataForForceGraph([d]);
		
		var force = d3.layout.force()
			.charge(-60)
			.linkDistance(30)
			.size([detailsDim, detailsDim])
			.linkStrength(1).nodes(graph.nodes).links(graph.links).start();
			
		var link = activityDetails.selectAll(".link")
		    .data(graph.links)
			.enter().append("line")
			.attr("class", "activities_link")
			.style("stroke-width", function(d) { return Math.sqrt(d.value); });  	
				  
		var node = activityDetails.selectAll(".node")
		    .data(graph.nodes)
		    .enter().append("circle")
			.call(force.drag);
		    .attr("r", 5);
		  		  
		node.append("title").html(function(d) {  return d.name  });//+ "<br>" + d.neighbours + " connections" ;});

		force.on("tick", function() { 
			link.attr("x1", function(d) { return d.source.x; })
			    .attr("y1", function(d) { return d.source.y; })
				.attr("x2", function(d) { return d.target.x; })
				.attr("y2", function(d) { return d.target.y; });
				
			node.attr("cx", function(d) { return d.x; })
			    .attr("cy", function(d) { return d.y; });
		});	  
	};
	
	return me;
}());
