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
	var notNetworkColor = "#000000";		
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

    var activityDetails;
	var detailsDim;
	me.showDetails = function(d){
		if  (!activityDetails) {
			detailsDim = $("#nciHistogramDetails").width();
		    activityDetails = chartDetails.append('svg')
				.attr('width', detailsDim)
			    .attr('height', detailsDim/2);
		} else {
			activityDetails.text("");
		};
		
	    $('#nciDetails').animate({
			scrollTop: 200
		}, 1500);
			
		if (d.Size > NCI.max_vertices)
		    return;	
	    var graph = NCI.prepareDataForForceGraph([d]);
		
		force = d3.layout.force()
			.charge(-60)
			.linkDistance(30)
			.size([detailsDim, detailsDim/2])
			.linkStrength(1).nodes(graph.nodes).links(graph.links).start();
			
		var link = activityDetails.selectAll(".link")
		    .data(graph.links)
			.enter().append("line")
			.attr("class", "activities_link")
			.style("stroke-width", function(d) { return Math.sqrt(d.value); });  	
				  
		chartDetails.node = activityDetails.selectAll(".node")
		    .data(graph.nodes)
		    .enter().append("circle")
			.call(force.drag)
		    .attr("r", 5)
			.style("fill", function(d) {
				return me.colorifyEndpoint(internalEndpointsCheckbox[0].checked, d);
			 });
		  		  
		chartDetails.node.append("title").html(function(d) {  
			var info = d.name;
			if (d.external){
				info += "<br>doesn't belong to activity";
			};
			info += "<br>" + d.connections + " connections";
			return info;  
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
		if ( devided && !endpoint.name.indexOf("10.") == 0 && !endpoint.name.indexOf("192.168") == 0
	     && !(endpoint.name.indexOf("172.") == 0 && parseInt(endpoint.name.substring(4, 6)) > 15 
		 && parseInt(endpoint.name.substring(4, 6)) < 32)){
			return notNetworkColor;
		}
		return color(0);
	};
	
	me.clean = function(){
		chartDetails.text("");
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
