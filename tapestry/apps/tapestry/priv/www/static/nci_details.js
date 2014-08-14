NCI.detailsNCI = $("#detailsNCI");
NCI.detailsTime = $("#detailsTime");
NCI.detailsFlows = $("#detailsFlows");
NCI.detailsEndpoints = $("#detailsEndpoints");
NCI.maxActivitySize = 0;
NCI.Social = {}

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
	
	NCI.Social.endpoints = 0;
	var sizesSum = 0;
	$.each(NCI.Communities, function(index, community){
		NCI.Social.endpoints += community.Endpoints.length;
		sizesSum  += community.Size;
	});
	
	NCI.detailsEndpoints.html(NCI.parceNumberForView(sizesSum));
};

$(".hide-ncidetails").on('click', function(){
	$('#nciDetails').removeClass('details-view-show');
	NCI.nciHistogram.clean();
	NCI.CommunityGraph = [];
	NCI.Communities = [];
	NCI.detailsEndpoints.html("");
	NCI.detailsNCI.html("");
	NCI.detailsTime.html("");
	NCI.detailsFlows.html("");
	$($('#nciDetailsTabs').find("dd a")[0]).click();
});


$('#nciDetailsTabs').on('toggled', function (event, tab) {
	switch(tab[0].id) {
	    case "panelFlows":
			var flowsPanel = new NCI.socialGraph("#panelFlows",{
				graphBuilder: new NCI.graphBuilder(NCI.Communities),
				numOfPoints: NCI.Social.endpoints
			});
	        flowsPanel.show(true);
	        break;
	    case "panelActivities":
			var activitiesPanel = new NCI.socialGraph("#panelActivities",{
				isDevided : true,
				isExpandable : true,
				numOfPoints: NCI.CommunityGraph.Endpoints.length,
				graphBuilder: new NCI.graphBuilder([NCI.CommunityGraph]),
				radius: function(endpoint){
					var radius = 4;
					var size = parseInt(endpoint.size);
				    if (NCI.maxActivitySize > 0 && (size == size)) {
						 radius = 4 + 8*(size/NCI.maxActivitySize);
				    }
					return radius;
				}
			});
			activitiesPanel.show(true);
	        break;
	    default:
			NCI.nciHistogram.show();
	};
});
