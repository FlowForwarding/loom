NCI.experementialGraph = function(){
	var socialGraphId = "#panelFlows";
	var me = $(socialGraphId);
	
	var graphWidth = me.width();
	var graphHeight =  $('#nciDetails').height() - 220;
	
	var experementialGraphId = "panelFlows_experemential"
	var me = $(socialGraphId);
	var socialGraphSelector = socialGraphId + " .socialGraph";
	var color = d3.scale.category10();
	
	var clean = function(){
		d3.select("#" + experementialGraphId).remove();
		$("#socialGraph").text("");
	};
	
	(function init(){
		clean();
	    me.experementialGraphSvg = d3.select(socialGraphSelector).append("svg")
		    .attr("id", experementialGraphId)
			.attr("width", graphWidth)
			.attr("height", graphHeight);
		
		
		var endpointsHash = {};
		
		
		var community = NCI.Communities[NCI.Communities.length - 1];
		var sizes = community.Sizes;
		var counter = 2;
		while (sizes === undefined) {
			community = NCI.Communities[NCI.Communities.length - counter];
			sizes = community.Sizes;
			counter++;
		}
		//console.log(community )
		var widthIndex = graphWidth/community.Sizes.width;
		var heightIndex = graphHeight/community.Sizes.height;
		
		//$.each(NCI.Communities, function(index, community){		
				$.each(community.GEndpoints, function(j, gendpoint){
					endpointsHash[gendpoint.Id] = [gendpoint.x, gendpoint.y]
					me.experementialGraphSvg.append("circle")
					    .attr("cx", function (d) { return gendpoint.x * widthIndex; })
					    .attr("cy", function (d) { return  gendpoint.y * heightIndex; })
					    .attr("r", function (d) { return 2; })
						.attr("name", function (d) { return gendpoint.name })
					    .style("fill", function(d) { return color(color(0)) });
		
				});

			$.each(community.Interactions, function(index, interaction){
				me.experementialGraphSvg.append("line")
				    .attr("x1", function (d) { return endpointsHash[interaction[0]][0] * widthIndex; })
				    .attr("y1", function (d) { return endpointsHash[interaction[0]][1] * heightIndex; })
				    .attr("x2", function (d) { return endpointsHash[interaction[1]][0] * widthIndex; })
					.attr("y2", function (d) { return endpointsHash[interaction[1]][1] * heightIndex; })
					.attr("stroke-width", 0.5)
                    .attr("stroke", "black");
			});
			//});
		
		//console.log(NCI.Communities)
	})();
	
}