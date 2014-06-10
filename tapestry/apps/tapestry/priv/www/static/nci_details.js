NCI.setupCommunities = function(data){
	NCI.Communities = data.Communities;
	NCI.CommunityGraph = data.CommunityGraph;
	NCI.Communities.sort(function(a, b){
		return a.Size- b.Size;
	});
	NCI.timestampNCI = data.NCI;
	NCI.timestamp = data.Time;
	$("#histogramGeneral").html("NETWORK COMPLEXITY INDEX at &nbsp;&nbsp; <i>" +
	 NCI.parceDateForLastUpdate(NCI.timestamp) + "</i>" +
	    "&nbsp;&nbsp;&nbsp;<span class='round alert label'>NCI " + NCI.timestampNCI + "</span>" );
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
			var sum = 0;
			$.each(NCI.Communities, function(index, community){
				sum += community.Size;
			});
		    
			if (sum > 300) {
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
		    if ( filtered && !(d.name.indexOf("10.") == 0 ||  d.name.indexOf("192.168") == 0)){
			    return notNetworkColor;
		    }
		    return devided ? color(d.group) : color(0);
		});
		me.node.attr("r", function(d) { return 4;})
		force.linkStrength(clustered ? 1 : 0);
	};
	
	me.draw = function(devided, clustered, filtered){	
		force = d3.layout.force()
		    .charge(-20)
			.linkDistance(30)
			.size([$('#nciDetails').width(),  $('#nciDetails').height() - 200 ])
			.linkStrength(clustered ? 1 : 0)
			.nodes(me.graph.nodes).links(me.graph.links).start();
			
	    me.activitiesGraphSvg = d3.select("#socialGraph").append("svg")
		    .attr("id","activities_graph")
			.attr("width", $('#nciDetails').width())
			.attr("height", $('#nciDetails').height() - 200);
	  
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
		me.node.append("title").html(function(d) { return d.name });//+ "<br>" + d.neighbours + " connections" ; });

	    force.on("tick", function() { 
	        link.attr("x1", function(d) { return d.source.x; })
	            .attr("y1", function(d) { return d.source.y; })
	            .attr("x2", function(d) { return d.target.x; })
	            .attr("y2", function(d) { return d.target.y; });

	        me.node.attr("cx", function(d) { return d.x; })
	            .attr("cy", function(d) { return d.y; });
		});
	}
	
	return me;
}());

NCI.prepareDataForForceGraph = function(communities){
    var graph = { "nodes":[], "links": []};
	var endpointsHash = {};
	
	var nodeCounter = 0;
	$.each(communities, function(index, community){
		$.each(community.Endpoints, function(index2, endpoint){
			endpointsHash[endpoint] = {index: nodeCounter};
			graph.nodes.push({
				"name": endpoint,
				"group": index
			});
			nodeCounter ++;
		});
	});	
	$.each(communities, function(index, community){	
		$.each(community.Interactions, function(index2, interacton){
			if (endpointsHash[interacton[1]] && endpointsHash[interacton[0]]){
			    var secondIndex = endpointsHash[interacton[1]].index;
			    var firstIndex = endpointsHash[interacton[0]].index;
			    graph.links.push({
				    "source": secondIndex,
				    "target": firstIndex,
				    "value": 1});
			};
		});
	});
	
	return graph;
};

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
