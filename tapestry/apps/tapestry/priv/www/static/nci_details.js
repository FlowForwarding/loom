NCI.detailsNCI = $("#detailsNCI");
NCI.detailsTime = $("#detailsTime");
NCI.detailsFlows = $("#detailsFlows");
NCI.detailsEndpoints = $("#detailsEndpoints");
NCI.detailsActivities = $("#detailsActivities");
NCI.maxActivitySize = 0;
NCI.Social = {}

NCI.setupCommunities = function(data){
	NCI.Communities = data.Communities;
    NCI.Lables = data.Labels;

	NCI.CommunityGraph = data.CommunityGraph;
	NCI.Communities.sort(function(a, b){
		return a.Size- b.Size;
	});

    var length = NCI.Communities.length;
    NCI.Communities.forEach(function(community, index) {
        community.NameIndex = length - index;
    });

    NCI.model.parseActivities(NCI.Communities);
    NCI.model.parseLabels(NCI.Lables);

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
//	NCI.nciHistogram.clean();
	NCI.CommunityGraph = [];
	NCI.Communities = [];
	NCI.detailsEndpoints.html("");
	NCI.detailsActivities.html("");
	NCI.detailsNCI.html("");
	NCI.detailsTime.html("");
	NCI.detailsFlows.html("");
	$($('#nciDetailsTabs').find("dd a")[2]).click();
});

NCI.showHostnames = false;

$("#settingsDropdown-checkbox").on("click", function() {
    var checked = this.checked;
    NCI.showHostnames = checked;
    $(NCI).trigger("showHostnames", checked);
});

NCI.detailsTabs = function(){
	var me = $('#nciDetailsTabs');
	var flowsPanel;
	var activitiesPanel;
    var activitiesSize;
    var endpointsView;
	var color = d3.scale.category10();
	
	me.on('toggled', function (event, tab) {
		if (activitiesPanel)
		    activitiesPanel.clean();
		if (flowsPanel)
			flowsPanel.clean();

        if (endpointsView) {
            endpointsView.stop();
        }

        $(window).trigger("resize");
		switch(tab[0].id) {
            case "panelEndpoints":
                if (!endpointsView) {
                    endpointsView = new NCI.EndpointsView($("#panelEndpoints"));
                }
                endpointsView.resume();

                break;
		    case "panelFlows":
				flowsPanel = new NCI.socialGraph("#panelFlows",{
					communities: NCI.Communities,
                    isFlows: true,
                    numOfPoints: NCI.Social.endpoints
				});
		        flowsPanel.show(true);
		        break;
		    case "panelActivities":
				activitiesPanel = new NCI.socialGraph("#panelActivities",{
					isDevided : true,
					isExpandable : true,
					legendData: [
                        [color(0), " - activity", "rect"],
                        [null, " - endpoint in activity"],
                        ["red", " - endpoint in a different activity"],
                        [NCI.notNetworkColor, " - external endpoint"]
                    ],
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
                var activities = NCI.model.activities();

                NCI.CommunityGraph.Endpoints.forEach(function (community) {
                    var tmp = community.split("|"),
                        size = tmp.pop(),
                        mainEndpointId = tmp.pop(),
                        activity = NCI.model.getActivityByMainEndpoint(mainEndpointId);

                    // TODO: get rid of hits hack;
                    if (activity) {
                        activity.size = parseInt(size, 10);
                    }

                });

                activities = activities.sort(function(a1, a2) {
                    var result = a2.size - a1.size;
                    if (!result) {
                        result = a1.index - a2.index;
                    }
                    return result;
                });

                if (!activitiesSize) {
                    activitiesSize = new NCI.NCIHistogram(d3.select("#nciHistogram"), activities, {
                        itemsPerPage: activities.length
                        //                    itemsPerPage: 20
                    });
                }
		};
	});
	
}();
