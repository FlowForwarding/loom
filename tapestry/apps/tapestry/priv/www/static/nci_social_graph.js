NCI.socialGraph = function(socialGraphID, params){
	if (!params)
	    params = {}
	var me = $(socialGraphID);
	var force;
	var selectedDots = {};
	var socialGraphSelector = socialGraphID + " .socialGraph";
	var legendSelector = socialGraphID + " .legend";
	var socialGraphID = socialGraphID.substring(1) + "_graph";
	var color = d3.scale.category10();
	var notNetworkColor = params.notNetworkColor || "#000000";
	var isClustered = params.isClustered || false ;
	var isDevided = params.isDevided || false ;
	var isFiltered = false;
	var graphWidth = params.width || me.width();
	var graphHeight =  params.height || $('#nciDetails').height() - 150;
	var radius = params.radius || function() { return 4};
	var charge = params.charge || function() { return -20};
	var linkDistance = params.linkDistance || function() { return 30};
	var graphBuilder = params.graphBuilder || undefined;
	var numOfPoints = params.numOfPoints || 0;
	var isExpandable = params.isExpandable || false;
	var tooltip;
	var byActivities = me.find('.byactivities');
	var prettyView = me.find('.pretty');
	var showInternal = me.find('.internal');
	
	byActivities.on('click', function(event){
		isDevided = this.checked;
		me.show();
	});
	prettyView.on('click', function(event){
		isClustered = this.checked;
		me.show();
	});
	showInternal.on('click', function(event){
		isFiltered = this.checked;
		me.show();
	});
	
	me.show = function(needDraw){
		
		if (tooltip === undefined)
			tooltip = d3.select(socialGraphSelector).append("div").attr("class", "endpoint-tooltip");
		if (NCI.Communities.length == 0){
			return;
		};
		if (needDraw) {
		    d3.select("#" + socialGraphID).remove();
			if (numOfPoints > NCI.max_vertices) {
				d3.select(socialGraphSelector).append('text')
				.attr("id", socialGraphID)
				.html('Too many endpoints to draw')
				.attr('class', 'centrate');
			} else {
			    me.draw();
			}
		//otherwise just change appearance	
		} else {
		    me.setupNodes();
		    force.start();
		}
	};
	
	//colorify and set radius
	me.setupNodes = function(){
		me.node.style("fill", function(d) {
		    if ( isFiltered && NCI.isExternal(d.name)){
			    return notNetworkColor;
		    };
			//if this dot is selected on Activities graph, draw it in red
			if (d.external)
			    return "red"
		    return isDevided ? color(d.group) : color(0);
		});
		me.node.attr("r", function(d) {
			return radius(d);
		});
		me.node.attr("height", function(d) { 
			return radius(d)*2;
		});
		me.node.attr("width", function(d) { 
			return radius(d)*2;
		});
		force.linkStrength(isClustered ? 1 : 0);
	};
	
	var zoom = d3.behavior.zoom()
	    .scaleExtent([1, 10])
	    .on("zoom", zoomed);
		
	function zoomed() {
		console.log(d3.event.translate)
		me.link.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
		me.node.attr("transform", "translate(" + d3.event.translate + ")scale(" + d3.event.scale + ")");
	};	
	
	me.draw = function(){	
		force = d3.layout.force()
		    .charge(charge())
			.linkDistance(linkDistance())
			.size([graphWidth,  graphHeight])
			.linkStrength(isClustered ? 1 : 0);
			
	    me.activitiesGraphSvg = d3.select(socialGraphSelector).append("svg")
		    .attr("id", socialGraphID)
			.attr("width", graphWidth)
			.attr("height", graphHeight).call(zoom);
			
		var setupLinks = function(){
			force.links(graphBuilder.graph.links);
			me.activitiesGraphSvg.selectAll("line").remove();
			var linksData = me.activitiesGraphSvg.selectAll(".link").data(graphBuilder.graph.links);
			me.link = linksData.enter().append("line")
		    .attr("class", "activities_link"); 
			linksData.exit().remove();
		};
		
		var setupNodes = function(){
			force.nodes(graphBuilder.graph.nodes);
			me.activitiesGraphSvg.selectAll("rect").remove();
			me.activitiesGraphSvg.selectAll("circle").remove();
			var nodesData =  me.activitiesGraphSvg.selectAll(".node").data(graphBuilder.graph.nodes);
			me.node = nodesData.enter().select(function(d) {
				var type = (isExpandable && d.group == 0) ? "rect" : "circle"
				return this.appendChild(document.createElementNS(d3.ns.prefix.svg, type));
			}).call(force.drag);
            nodesData.exit().remove();
			me.setupNodes(isFiltered, isDevided, isClustered);
			me.node.on('mouseover', function(d){
				var info = d.name;
				if (d.size){
					info += "<br>size : " + d.size;
				};
				info += "<br>connections : " + d.connections;
				tooltip.html(info).style("top", d.py + me.position().top).
				style("left", d.px + me.position().left).style("display", "inline");
			}).on('mouseout', function(){
				tooltip.style("display", "none");
			});
			if (isExpandable) {
				me.node.on('click', function(d){
					var label = d.name.split(":")[0];
					var group = selectedDots[label]
					if (group !== undefined){
						delete selectedDots[selectedDots[label]]
						delete selectedDots[label]
						graphBuilder.removeCommunity(group);
						setupLinks();		
						setupNodes();
						force.start();
						return;
					}
					for (var i=0; i < Object.keys(selectedDots).length + 1; i++){
						if (selectedDots[i + 1] === undefined){
							selectedDots[i + 1] = true;
							selectedDots[label] =  i + 1;
							break
						}
					}
					$.each(NCI.Communities, function(index, community){
						if (community.Label == label){
							graphBuilder.addCommunity(community, label, selectedDots[label] );
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
				
			me.node.attr("x", function(d) { return d.x - radius(d); })
			    .attr("y", function(d) { return d.y - radius(d); });
			me.node.attr("cx", function(d) { return d.x; })
			    .attr("cy", function(d) { return d.y; });
		});
		force.start();
	};
	
	me.clean = function(){
		d3.select("#" + socialGraphID).remove();
		byActivities.prop('checked', false);
		prettyView.prop('checked', false);
		showInternal.prop('checked', false);
	}
	
	me.setupLegend = function(legend_data){
		var dim = 15;
		var lineHeight = 30;
		var legend = d3.select(legendSelector).append("svg");
		$.each(legend_data, function(index, line){
		 	legend.append("rect").attr("height", dim).attr("width", dim).attr("y", lineHeight*index).attr("fill", line[0]);
		 	legend.append("text").html("- " + line[1] + "<br/>").attr("x", 30).attr("fill", notNetworkColor).attr("y", lineHeight*index + dim);
		});
	};
	
	if (params.legendData && $(legendSelector + " svg").length === 0){
		me.setupLegend(params.legendData);
	};
	
	return me;
};

NCI.graphBuilder = function(communities){
	var thisBuilder = this;
	var endpointsHash = {};
	var groupCount = 0;
    thisBuilder.graph = { "nodes":[], "links": []};
	NCI.maxActivitySize = 0;
	
	//add community
	thisBuilder.addCommunity = function(community, mainEndpoint, group){
		if (community.Endpoints.length > NCI.max_vertices)
		    return
		var communityEndpoints = {};
		var startIndex = Object.keys(endpointsHash).length
		var addConnection = function(endPoint, endpoints){
			var ip = endPoint.split(":")[0];
			if (!endpointsHash[ip]){
			    if (!communityEndpoints[ip]){
					var size = endPoint.split(":")[1];
				    var index = Object.keys(communityEndpoints).length + startIndex
				    communityEndpoints[ip] = {
						fullname: endPoint,
					    index: index,
					    external: endpoints.indexOf(endPoint) == -1,
					    connections: 0,
						size: size};
			    };
			    communityEndpoints[ip].connections++;
			    return communityEndpoints[ip].index;
		    } else {
				endpointsHash[ip].external = false;
		    	return endpointsHash[ip].index;
		    }
		};
		
		$.each(community.Interactions, function(index, interacton){
			thisBuilder.graph.links.push({
				source: addConnection(interacton[0], community.Endpoints),
				target: addConnection(interacton[1], community.Endpoints),
				value: 1});
		});
		
		$.each(Object.keys(communityEndpoints), function(index, key){
			if (mainEndpoint == key)
			   return
			var endpoint = communityEndpoints[key];
			if (NCI.maxActivitySize < parseInt(endpoint.size))
			    NCI.maxActivitySize = parseInt(endpoint.size);
			thisBuilder.graph.nodes[endpoint.index] = {
				name: key,
				fullname: endpoint.fullname,
				group: group,
				connections: endpoint.connections,
				external: endpoint.external,
				size: endpoint.size
			};
			endpointsHash[key] = communityEndpoints[key];
		});	
	};
	
	//remove community
	thisBuilder.removeCommunity = function(group){
	    thisBuilder.graph.nodes = $.grep(thisBuilder.graph.nodes, function(node, index){
			var remove = node.group == group
			if (remove)
			  delete endpointsHash[node.name];
			return !remove;
		});
	
	    thisBuilder.graph.links = $.grep(thisBuilder.graph.links, function(link, index){
			var remove = endpointsHash[link.target.name] === undefined || endpointsHash[link.source.name] === undefined;
			return !remove;
		});
		groupCount--;
	};
	
	$.each(communities, function(index, community){	
		thisBuilder.addCommunity(community, undefined, groupCount);
		groupCount++;
	});
};