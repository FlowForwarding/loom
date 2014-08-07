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
	var sizesSum = 0;
	$.each(NCI.Communities, function(index, community){
		NCI.socialGraph.endpoints += community.Endpoints.length;
		sizesSum  += community.Size;
	});
	
	NCI.detailsEndpoints.html(NCI.parceNumberForView(sizesSum));
};

NCI.socialGraph  = (function(){
	var me = $("#socialGraph");
	
	var force;
	var color = d3.scale.category10();
	var notNetworkColor = "#000000";
	var isClustered = false;
	var isActivities = false;
	var isDevided = false;
	var isFiltered = false;
	
	me.show = function(devided, clustered, filtered, activities){
		if (NCI.Communities.length == 0){
			return;
		};	
		//if we change type of graph (flows/activities) or need to redraw it from sratch
		if (isActivities != activities || !NCI.socialGraph.graph || (isClustered && !clustered)){
		    d3.select("#activities_graph").remove();
			var numOfPoints =  activities ?  NCI.CommunityGraph.Endpoints.length : NCI.socialGraph.endpoints;
			if (numOfPoints > NCI.max_vertices) {
				d3.select("#activities_graph").remove();
				d3.select("#socialGraph").append('text')
				.attr("id","activities_graph")
				.html('Too many endpoints to draw');
			} else {
				activities ? NCI.buildGraphData([NCI.CommunityGraph]) : NCI.buildGraphData(NCI.Communities);
				isClustered = clustered;
				isActivities = activities;
				isDevided = devided;
				isFiltered = filtered;
			    me.draw(devided, clustered, filtered);
				isClustered = clustered;
			}
		//otherwise just change appearance	
		} else {
			isClustered = clustered;
			isActivities = activities;
			isDevided = devided;
			isFiltered = filtered;
		    me.setupNodes(filtered, devided, clustered);
		    force.start();
		}
	};
	
	//colorify and set radius
	me.setupNodes = function(filtered, devided, clustered){
		me.node.style("fill", function(d) {
			//if this dot is selected on Activities graph, draw it in red
			if (NCI.Social.selectedDot == d.name.split(":")[0])
			    return "FF0000";
		    if ( filtered && NCI.isExternal(d.name)){
			    return notNetworkColor;
		    };
		    return devided ? color(d.group) : color(0);
		});
		me.node.attr("r", function(d) { 
			var radius = 4;
			if (NCI.Social.selectedDot == d.name.split(":")[0])
			    radius = 6;
			if (NCI.maxActivitySize > 0 && d.size) {
		 		radius = 4 + 8*(d.size/NCI.maxActivitySize);
		 	}
			return radius;
		});
		force.linkStrength(clustered ? 1 : 0);
	};
	
	me.draw = function(devided, clustered, filtered){	
		var graphWidth = me.width();
		var graphHeight = $('#nciDetails').height() - 200;
		force = d3.layout.force()
		    .charge(-20)
			.linkDistance(30)
			.size([graphWidth,  graphHeight])
			.linkStrength(clustered ? 1 : 0);
			
	    me.activitiesGraphSvg = d3.select("#socialGraph").append("svg")
		    .attr("id","activities_graph")
			.attr("width", graphWidth)
			.attr("height", graphHeight);
			
		var setupLinks = function(){
			force.links(me.graph.links);
			me.activitiesGraphSvg.selectAll("line").remove();
			var linksData = me.activitiesGraphSvg.selectAll(".link").data(me.graph.links);
			me.link = linksData.enter().append("line")
		    .attr("class", "activities_link"); 
			linksData.exit().remove();
		};
		
		NCI.Social.selectedDots = []
		var setupNodes = function(){
			force.nodes(me.graph.nodes);
			me.activitiesGraphSvg.selectAll("circle").remove();
			var nodesData =  me.activitiesGraphSvg.selectAll(".node").data(me.graph.nodes);
            me.node = nodesData.enter().append("circle").call(force.drag);
            nodesData.exit().remove();
			me.setupNodes(isFiltered, isDevided, isClustered);
			me.node.append("title").html(function(d) { return d.name + "<br>" + d.connections + " connections" ; });
			if (isActivities) {
				me.node.on('click', function(d){
					var label = d.name.split(":")[0];
					var group = NCI.Social.selectedDots.indexOf(label) + 1;
					if (group > 0){
						NCI.Social.selectedDots.splice(group - 1, 1)
						NCI.Social.removeCommunity(group);
						setupLinks();		
						setupNodes();
						force.start();
						return;
					}
					NCI.Social.selectedDots.push(label);
					$.each(NCI.Communities, function(index, community){
						if (community.Label == label){
							NCI.buildGraphData.addCommunity(community, label);
							setupLinks();		
							setupNodes();
							force.start();
							return false;
						}
					});
	 		    });
			};
		};
		
		setupLinks();
		setupNodes();
	    force.on("tick", function() { 
	        me.link.attr("x1", function(d) { return d.source.x; })
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

NCI.Social = {}

NCI.Social.removeCommunity = function(group){
    NCI.socialGraph.graph.nodes = $.grep(NCI.socialGraph.graph.nodes, function(node, index){
		var remove = node.group == group
		if (remove)
		  delete NCI.Social.endpointsHash[node.name];
		return !remove;
	});
	
    NCI.socialGraph.graph.links = $.grep(NCI.socialGraph.graph.links, function(link, index){
		var remove = NCI.Social.endpointsHash[link.target.name] === undefined || NCI.Social.endpointsHash[link.source.name] === undefined
		return !remove
	});
	NCI.socialGraph.groupNumber--;
};

NCI.buildGraphData = function(communities){
    NCI.socialGraph.graph = { "nodes":[], "links": []};
    NCI.socialGraph.groupNumber = 0;
	NCI.Social.endpointsHash = {};
	NCI.maxActivitySize = 0;
	
	NCI.buildGraphData.addCommunity = function(community, mainEndpoint){
		var communityEndpoints = {};
		var startIndex = Object.keys(NCI.Social.endpointsHash).length
		var addConnection = function(endPoint, group, endpoints){
			var ip = endPoint.split(":")[0];
			if (!NCI.Social.endpointsHash[ip]){
			    if (!communityEndpoints[ip]){
					var size = endPoint.split(":")[1];
				    var index = Object.keys(communityEndpoints).length + startIndex
				    communityEndpoints[ip] = {
					    index: index,
					    external: !endpoints.indexOf(endPoint),
					    connections: 0,
						size: size,
				        group: group};
			    };
			    communityEndpoints[ip].connections++;
			    return communityEndpoints[ip].index;
		    } else {
		    	return NCI.Social.endpointsHash[ip].index;
		    }
		};
		
		$.each(community.Interactions, function(index, interacton){
			NCI.socialGraph.graph.links.push({
				source: addConnection(interacton[0], NCI.socialGraph.groupNumber, community.Endpoints),
				target: addConnection(interacton[1], NCI.socialGraph.groupNumber, community.Endpoints),
				value: 1});
		});
		
		$.each(Object.keys(communityEndpoints), function(index, key){
			if (mainEndpoint == key)
			   return
			var endpoint = communityEndpoints[key];
			if (NCI.maxActivitySize < parseInt(endpoint.size))
			    NCI.maxActivitySize = parseInt(endpoint.size);
			NCI.socialGraph.graph.nodes.push({
				index: endpoint.index,
				name: key,
				group: endpoint.group,
				connections: endpoint.connections,
				external: endpoint.external,
				size: endpoint.size
			});
			NCI.Social.endpointsHash[key] = communityEndpoints[key]
		});	
		NCI.socialGraph.groupNumber++
	};
	
	$.each(communities, function(index, community){	
		NCI.buildGraphData.addCommunity(community)
	});
	
	return NCI.socialGraph.graph;
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
