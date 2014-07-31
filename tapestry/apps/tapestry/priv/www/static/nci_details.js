NCI.detailsNCI = $("#detailsNCI");
NCI.detailsTime = $("#detailsTime");
NCI.detailsFlows = $("#detailsFlows");
NCI.detailsEndpoints = $("#detailsEndpoints");
NCI.maxActivitySize = 0;

NCI.setupCommunities = function(data){
	NCI.Communities = data.Communities;
	NCI.CommunityGraph = data.CommunityGraph;
	NCI.Communities.sort(function(a, b){
		return a.Size- b.Size;
	});
	NCI.timestampNCI = data.NCI;
	NCI.timestamp = data.Time;

	NCI.detailsNCI.html(NCI.timestampNCI);
	NCI.detailsTime.html(NCI.parceDateForLastUpdate(NCI.timestamp));
	
	NCI.socialGraph.endpoints = 0;
	$.each(NCI.Communities, function(index, community){
		NCI.socialGraph.endpoints += community.Endpoints.length;
	});
	NCI.detailsEndpoints.html(NCI.socialGraph.endpoints);
};

NCI.socialGraph  = (function(){
	var me = $("#socialGraph");
	
	var force;
	var color = d3.scale.category10();
	var notNetworkColor = "#000000";
	var isClustered = false;
	var isCommunities = false;
	
	me.show = function(devided, clustered, filtered, communities){
		if (NCI.Communities.length == 0){
			return;
		};		
		if (communities) {
			if (isCommunities != communities || !me.graph || (isClustered && !clustered)){
				d3.select("#activities_graph").remove();
				me.graph = NCI.prepareDataForForceGraph([NCI.CommunityGraph]);
				me.draw(devided, clustered, filtered);
			} else {
				me.setupNodes(filtered, devided, clustered);
				force.start();
			}
		} else {
			if (NCI.socialGraph.endpoints > NCI.max_vertices) {
				d3.select("#activities_graph").remove();
				d3.select("#socialGraph")
				.append('text')
				.attr("id","activities_graph")
				.html('Too many endpoints to draw');
			} else {
			    if (isCommunities != communities || !me.graph || (isClustered && !clustered)){
				    d3.select("#activities_graph").remove();
				    me.graph = NCI.prepareDataForForceGraph(NCI.Communities);
				    me.draw(devided, clustered, filtered);
			    } else {
				    me.setupNodes(filtered, devided, clustered);
				    force.start();
			    };
		    }
		};
		isCommunities = communities;
		isClustered = clustered;
	};
	
	me.setupNodes = function(filtered, devided, clustered){
		me.node.style("fill", function(d) { 
		    if ( filtered && NCI.isExternal(d.name)){
			    return notNetworkColor;
		    };
		    return devided ? color(d.group) : color(0);
		});
		me.node.attr("r", function(d) { 
			if (NCI.maxActivitySize > 0){
				var radius = 4 + 8*(d.size/NCI.maxActivitySize);
			} else {
				var radius = 4;
			}
			return radius;
		});
		force.linkStrength(clustered ? 1 : 0);
	};
	
	me.draw = function(devided, clustered, filtered){	
		var graphWidth = $('#socialGraph').width();
		var graphHeight = $('#nciDetails').height() - 200;
		force = d3.layout.force()
		    .charge(-20)
			.linkDistance(30)
			.size([graphWidth,  graphHeight])
			.linkStrength(clustered ? 1 : 0)
			.nodes(me.graph.nodes).links(me.graph.links);
			
	    me.activitiesGraphSvg = d3.select("#socialGraph").append("svg")
		    .attr("id","activities_graph")
			.attr("width", graphWidth)
			.attr("height", graphHeight);
	  
		var link = me.activitiesGraphSvg.selectAll(".link")
		    .data(me.graph.links)
			.enter().append("line")
			.attr("class", "activities_link")
			.style("stroke-width", function(d) { return Math.sqrt(d.value); });  	
		  
		me.node = me.activitiesGraphSvg.selectAll(".node")
		    .data(me.graph.nodes)
			.enter().append("circle")
			.call(force.drag);
		me.setupNodes(filtered, devided, clustered);
		me.node.append("title").html(function(d) { return d.name + "<br>" + d.connections + " connections" ; });

	    force.on("tick", function() { 
	        link.attr("x1", function(d) { return d.source.x; })
	            .attr("y1", function(d) { return d.source.y; })
	            .attr("x2", function(d) { return d.target.x; })
	            .attr("y2", function(d) { return d.target.y; });

	        me.node.attr("cx", function(d) { return d.x; })
	            .attr("cy", function(d) { return d.y; });
		});
		force.start();
	};
	
	return me;
}());

NCI.prepareDataForForceGraph = function(communities){
    var graph = { "nodes":[], "links": []};
	var endpointsHash = {};
	
	var addConnection = function(endPoint, group, endpoints){
		if (!endpointsHash[endPoint]){
			endpointsHash[endPoint] = {index: Object.keys(endpointsHash).length,
				external : !endpoints.indexOf(endPoint),
				connections: 0,
			    group: group};
		}
		endpointsHash[endPoint].connections++;
		return endpointsHash[endPoint].index;
	};
	
	$.each(communities, function(index, community){	
		$.each(community.Interactions, function(index2, interacton){
			graph.links.push({
				"source": addConnection(interacton[0], index, community.Endpoints),
				"target": addConnection(interacton[1], index, community.Endpoints),
				"value": 1});
		});
	});
	
	NCI.maxActivitySize = 0;
	$.each(Object.keys(endpointsHash), function(index, key){
		var endpoint = endpointsHash[key];
		var semiIndex = key.search(":");
		var size = (semiIndex >= 0) ? key.substring(semiIndex + 1) : 0;
		if (NCI.maxActivitySize < parseInt(size))
		    NCI.maxActivitySize = parseInt(size);
		graph.nodes.push({
			"name": key,
			"group": endpoint.group,
			"connections": endpoint.connections,
			"external": endpoint.external,
			"size": size
		});
	});	
	
	return graph;
};

$(".hide-ncidetails").on('click', function(){
	$('#nciDetails').removeClass('details-view-show');
	NCI.nciHistogram.clean();
	NCI.socialGraph.text("");
	NCI.CommunityGraph = [];
	NCI.Communities = [];
	NCI.detailsEndpoints.html("");
	NCI.detailsNCI.html("");
	NCI.detailsTime.html("");
	NCI.detailsFlows.html("");
	NCI.socialGraph.graph = undefined;
	$($('#nciDetailsTabs').find("dd a")[0]).click();
});


$('#nciDetailsTabs').on('toggled', function (event, tab) {
	switch(tab[0].id) {
	    case "panelFlows":
	        NCI.socialGraph.show( false, false, false, false);
	        break;
	    case "panelFlowsByActivities":
	        NCI.socialGraph.show(true, false, false, false);
	        break;
	    case "panelFlowsPretty":
	        NCI.socialGraph.show(true, true, false, false);
	        break;
	    case "panelInternalFlows":
	        NCI.socialGraph.show(true, true, true, false);
	        break;
	    case "panelActivities":
	        NCI.socialGraph.show(false, false, false, true);
	        break;
	    case "panelActivitiesPretty":
	        NCI.socialGraph.show(true, true, false, true);
	        break;
	    case "panelActivitiesInternal":
	        NCI.socialGraph.show(true, true, true, true);
	        break;				
	    default:
			NCI.nciHistogram.show();
			NCI.socialGraph.text("");
			NCI.socialGraph.graph = undefined;
	};
});
