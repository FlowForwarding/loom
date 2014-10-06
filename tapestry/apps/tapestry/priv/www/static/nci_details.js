NCI.detailsNCI = $("#detailsNCI");
NCI.detailsTime = $("#detailsTime");
NCI.detailsFlows = $("#detailsFlows");
NCI.detailsEndpoints = $("#detailsEndpoints");
NCI.detailsActivities = $("#detailsActivities");
NCI.maxActivitySize = 0;
NCI.Social = {}

NCI.setupCommunities = function(data){
	console.log(data)
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
	NCI.detailsActivities.html(NCI.parceNumberForView(NCI.CommunityGraph.Endpoints.length));
	$("#loading_label").remove();
};

$(".hide-ncidetails").on('click', function(){
	$('#nciDetails').removeClass('details-view-show');
	NCI.nciHistogram.clean();
	NCI.CommunityGraph = [];
	NCI.Communities = [];
	NCI.detailsEndpoints.html("");
	NCI.detailsActivities.html("");
	NCI.detailsNCI.html("");
	NCI.detailsTime.html("");
	NCI.detailsFlows.html("");
	$($('#nciDetailsTabs').find("dd a")[2]).click();
});

NCI.detailsTabs = function(){
	var me = $('#nciDetailsTabs');
	var flowsPanel;
	var activitiesPanel;
	var color = d3.scale.category10();
	
	me.on('toggled', function (event, tab) {
		if (activitiesPanel)
		    activitiesPanel.clean()
		if (flowsPanel)
			flowsPanel.clean()
		switch(tab[0].id) {
		    case "panelFlows":
				flowsPanel = new NCI.socialGraph("#panelFlows",{
					communities: NCI.Communities,
                    numOfPoints: NCI.Social.endpoints
				});
		        flowsPanel.show(true);
		        break;
		    case "panelActivities":
				activitiesPanel = new NCI.socialGraph("#panelActivities",{
					isDevided : true,
					isExpandable : true,
					legendData : [[color(0), "- activity", "rect"],
					    ["red", "- endpoint in a different activity"],
					    ["#000000", "- external endpoint"],
						["", "all other colored dots - endpoints in an activity", "none"]],
					numOfPoints: NCI.CommunityGraph.Endpoints.length,
					communities: [NCI.CommunityGraph],
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
	
}();
