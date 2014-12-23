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
	var notNetworkColor = NCI.notNetworkColor;
	var isClustered = params.isClustered || false ;
	var isDevided = params.isDevided || false ;
	var isFiltered = false;
    var isFlows = params.isFlows || false;
	var graphWidth = params.width || me.width();
	var graphHeight =  params.height || $('#nciDetails').height() - 150;
	var radius = params.radius || function() { return 4};
	var charge = params.charge || function() { return -20};
	var linkDistance = params.linkDistance || function() { return 30};
    var communities = params.communities || null;
	var graphBuilder = communities ? new NCI.graphBuilder(communities, isFlows) : undefined;
    var listBuilder = NCI.list.createListBuilder(communities);
	var numOfPoints = params.numOfPoints || 0;
	var isExpandable = params.isExpandable || false;
	var tooltip;
	var byActivities = me.find('.byactivities');
	var prettyView = me.find('.pretty');
	var showInternal = me.find('.internal');
	var experimentalView = me.find('.experimental');

    var $showOutside = me.find('.outside');
    var $showList = me.find('.show-list');
    var $exportList = me.find('.export-list');
    var $showGraph = me.find('.show-graph');
    var $activitiesList = me.find(".activities-list");
    var $endpointFilter = me.find(".endpoint-filter");

	var tmpLine = undefined;
    var showHostname = NCI.showHostnames;

    $(NCI).on("showHostnames", function(event, show) {
        showHostname = show;
    });
	
	byActivities.on('click', function(event){
		isDevided = this.checked;
		me.show(false, false);
	});
	prettyView.on('click', function(event){
		isClustered = this.checked;
		me.show(false, true);
	});
	showInternal.on('click', function(event){
		isFiltered = this.checked;
		me.show(false, false);
	});

	experimentalView.on('click', function(event){
	    var experementialGraph = new NCI.experementialGraph();
		me.clean();
	});

    $showOutside.on("click", function() {
        var showOutside = this.checked;
        graphBuilder = new NCI.graphBuilder(communities, showOutside);
        me.show(true, prettyView.prop("checked"));
    });

    function downloadActivityList() {
        listBuilder.downloadCSV();
    }

    $exportList.on("click", downloadActivityList);

    $endpointFilter.on("keyup", function() {
        listBuilder.filterTableByEndpoint($(this).val());
    });

    if ($activitiesList.length > 0) {
        $showGraph.click(function() {
            showListView(false);
        });
        $showList.click(function() {
            showListView(true);
        });
        showInternal.on('click', function(event) {
            listBuilder.filterTableByInternal(this.checked);
        });

        var activitiesList = d3.select($activitiesList.get(0));

        listBuilder.createTable(activitiesList);
    }


    function showListView(state) {
        // state == true to show ListView
        // false to show graph
        $showList.parent().toggleClass("hide", state);

        $endpointFilter.parent().toggleClass("hide", !state);
        $showGraph.parent().toggleClass("hide", !state);

        byActivities.parent().toggleClass("hide", state);
        prettyView.parent().toggleClass("hide", state);
        experimentalView.parent().toggleClass("hide", state);

        $showOutside.parent().toggleClass("hide", state);


        $(socialGraphSelector).toggle(!state);
        $(legendSelector).toggle(!state);

        $activitiesList.toggle(state);

    }

	me.show = function(needDraw, needForce){
		if (tooltip === undefined)
			tooltip = d3.select(socialGraphSelector).append("div").attr("class", "endpoint-tooltip");
		if (NCI.Communities.length == 0){
			return;
		};
		if (needDraw) {
		    d3.select("#" + socialGraphID).remove();
            $(".too-many-msg").remove();
			if (numOfPoints > NCI.max_vertices) {
				d3.select(socialGraphSelector).append('text')
				.attr("id", socialGraphID)
				.html('Too many flows to draw')
				.attr('class', 'centrate');

                // if there is too many endpoints to show, disable show graph functionality
                showListView(true);
                $showGraph.parent().addClass("disabled");
                $showGraph.off("click");

			} else if (numOfPoints === 0) {
                // this means that we haven't receive endpoints from BE, because of limits

                $(socialGraphSelector).append('<div class="too-many-msg">Too many endpoints</div>');

            } else {
			    me.draw();
				NCI.GraphAppearsSound.currentTime = 0;
				NCI.GraphAppearsSound.play();	
			}
		//otherwise just change appearance	
		} else {
		    me.setupNodes();
			var sound;
			if (needForce) {
				sound = isClustered ? NCI.PrettyOn : NCI.PrettyOff;
				force.start();
			} else {
				sound =isFiltered ? NCI.ExternalOn : NCI.ExternalOff;
			}
			sound.currentTime = 0;
			sound.play();
		}
	};
	
	//colorify and set radius
	me.setupNodes = function(){
        if (!me || !me.node) {
            return;
        }
		me.node.style("fill", function(d) {
            // TODO: review d.size, since right now it's only way to 100% detect that this is Activity node
		    if ( isFiltered && NCI.isExternal(d.name) && d.size==undefined){
			    return notNetworkColor;
		    };
			//if this dot is selected on Activities graph, draw it in red
			if (d.external && !isFlows)
			    return "red";

            if (d.size && d.clicked) {
                return "#1D5082";
            }

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
	    .on("zoom", zoomed).on("zoomend", zoomend);
		
	function zoomend() {
		if (tmpLine !== undefined) {
			tmpLine = undefined;
			graphBuilder.graph.links.pop();
		}
	}	
	
	function zoomed() {
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
		    .attr("class", function(d){ return d.tmp ? "activities_link_tmp" : "activities_link"});
			linksData.exit().remove();
		};
		
		var setupNodes = function(){
			force.nodes(graphBuilder.graph.nodes);
			me.activitiesGraphSvg.selectAll("rect").remove();
			me.activitiesGraphSvg.selectAll("circle").remove();
			var nodesData =  me.activitiesGraphSvg.selectAll(".node").data(graphBuilder.graph.nodes);
			me.node =
                nodesData
                    .enter()
                    .append(function(d) {
                        var type = (isExpandable && d.group == 0) ? "rect" : "circle"
                        return document.createElementNS(d3.ns.prefix.svg, type);
        			}).call(force.drag);

            nodesData
                .exit()
                .remove();

			me.setupNodes(isFiltered, isDevided, isClustered);
			me.node.on('mouseover', function(d){
				var soundName = NCI.MouseOverBlueSquare;
				if (!(isExpandable && d.group == 0))  {
					if (isFiltered && NCI.isExternal(d.name)) {
						soundName = NCI.MouseOverExternalEndpoint;
					} else if (d.external && !(isFiltered && NCI.isExternal(d.name))) {	
						soundName = NCI.MouseOverRedDot;
					} else if (d.group == 0){
						soundName = NCI.MouseOverBlueDot;
					} else {
						soundName = undefined;
					};
				} else if ( d.group == 0 && isFiltered && NCI.isExternal(d.name)) {
                    // TODO: since we don't have black squre, do we need special sound for that?
					soundName = NCI.MouseOverBlackSquare;
				};
				if (soundName !== undefined){
					soundName.play();	
				};
				var info = d.label ? d.label + "</br>" : "";
                info += d.name;

                info += (showHostname && !d.size) ? ("</br>" + NCI.model.hostNameForIp(d.name)) : "";

				if (d.size){
					info += "<br>size : " + d.size;
				};
				info += "<br>connections : " + d.connections;
				var newx = this.getCTM().e;
				var newy = this.getCTM().f;
				if (this.getCTM().d != 0) {
					newx = newx / this.getCTM().d;
					newy = newy / this.getCTM().d;
				}
                // TODO: calculate this values
                var leftAdjustment = -50;

				tooltip.html(info)
                    .style("display", "inline")
                    .style("top", d.py + newy + me.position().top - tooltip.node().offsetHeight/2 + 5)
                    .style("left", d.px + newx + me.position().left + leftAdjustment);
			}).on('mouseout', function(){
				tooltip.style("display", "none");
			});
			if (isExpandable) {
				me.node.on('click', function(d){
					var label = d.name.split("|")[0];
					var group = selectedDots[label];
                    d.clicked = !d.clicked;
					if (group !== undefined){
						delete selectedDots[selectedDots[label]];
						delete selectedDots[label];
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
							var added = graphBuilder.addCommunity(community, label, selectedDots[label] );
                            if (added !== false) {
                                NCI.MouseClickActivitySound.currentTime = 0;
                                NCI.MouseClickActivitySound.play();
                            }
                            setupLinks();
                            setupNodes();
                            force.start();
                            return false;
						}
					});
	 		    }).on('mousedown', function(d){
				if (d.external) {
					//TODO  find right node!!! instead of graphBuilder.graph.nodes[0]
					$.each(NCI.Communities, function(index, community){
						var connectedNode = undefined
						if (community.Endpoints.indexOf(d.name) >= 0){
							$.each(graphBuilder.graph.nodes, function(index, node){
								if (node.name == community.Label){
									connectedNode = node;
									return;
								}
							});
							tmpLine = {
								source: connectedNode,
								target: d,
								tmp: true,
								value: 1};
							graphBuilder.graph.links.push(tmpLine);
							setupLinks();		
							setupNodes();
							force.start();
							return;
						}
					});
				}
			}).on('mouseup', function(d){
				if (d.external) {
					if (tmpLine !== undefined) {
						tmpLine = undefined;
						graphBuilder.graph.links.pop();
						setupLinks();		
						setupNodes();
						force.start();
					}
				}
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

        showInternal.off("click");

        $showOutside.prop("checked", false);
        $showOutside.off("click");

        $showGraph.off("click");
        $showList.off("click");
        $exportList.off("click", downloadActivityList)

        $showGraph.parent().removeClass("disabled");

        $endpointFilter.off("keyup");
        $endpointFilter.val("");
        listBuilder.removeTable();

        if (force) {
            force.stop();
        }

        showListView(false);
	}
	
	me.setupLegend = function(legend_data){
		var dim = 16;
		var lineHeight = 30;
		var legend = d3.select(legendSelector).append("svg").attr("width", 400);
		$.each(legend_data, function(index, line){
            var shape = line[2],
                shapeColor = line[0],
                text = line[1];

			switch(shape) {
			    case "rect":
					legend.append("rect").attr("height", dim).attr("width", dim).attr("fill", shapeColor);
					break;
				case "none":
                    break;
                default:
				 	var circle = legend.append("circle").attr("r", dim/2).attr("cy", lineHeight*index + 10)
                        .attr("cx", dim/2).attr("fill", shapeColor);

                    if (shapeColor == null) {
                        var i = 3,
                            duration = 1000;

                        setInterval(function() {
                            circle
                                .transition()
                                .duration(duration)
                                .attr("fill", color(i++));
                        }, duration);
                    }

			}
			legend.append("text").html(text + "<br/>").attr("x", 30).attr("y", lineHeight*index + dim);
		});
	};
	
	if (params.legendData && $(legendSelector + " svg").length === 0){
		me.setupLegend(params.legendData);
	};
	
	return me;
};

NCI.graphBuilder = function(communities, includeOutside){
	var thisBuilder = this;
	var endpointsHash = {};
	var groupCount = 0;
    thisBuilder.graph = { "nodes":[], "links": []};
	NCI.maxActivitySize = 0;
	
	//add community
	thisBuilder.addCommunity = function(community, mainEndpoint, group, addLabel){
		if (community.Endpoints.length > NCI.max_vertices)
		    return false;
		var communityEndpoints = {};
		var startIndex = Object.keys(endpointsHash).length
		var addConnection = function(endPoint, endpoints){
			var ip = endPoint.split("|")[0];
			if (!endpointsHash[ip]){
			    if (!communityEndpoints[ip]){
					var size = endPoint.split("|")[1];
				    var index = Object.keys(communityEndpoints).length + startIndex
				    communityEndpoints[ip] = {
						fullname: endPoint,
					    index: index,
                        communityLabel: addLabel ?
                            "Activity #" + (NCI.CommunityGraph.Endpoints.indexOf(endPoint) + 1) : null,
					    external: isOutside(endPoint),
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

        function isOutside(endpoint) {
            return community.Endpoints.indexOf(endpoint) == -1;
        }
		
		$.each(community.Interactions, function(index, interacton){
            if (includeOutside || !(isOutside(interacton[0]) || isOutside(interacton[1]))) {
                thisBuilder.graph.links.push({
                    source: addConnection(interacton[0], community.Endpoints),
                    target: addConnection(interacton[1], community.Endpoints),
                    value: 1
                });
            }
		});
		
		$.each(Object.keys(communityEndpoints), function(index, key){
			if (mainEndpoint == key)
			   return
			var endpoint = communityEndpoints[key];
			if (NCI.maxActivitySize < parseInt(endpoint.size))
			    NCI.maxActivitySize = parseInt(endpoint.size);
			thisBuilder.graph.nodes[endpoint.index] = {
				name: key,
                label: endpoint.communityLabel,
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
        var addActivityLabel = community === NCI.CommunityGraph;
		thisBuilder.addCommunity(community, undefined, groupCount, addActivityLabel);
		groupCount++;
	});
};