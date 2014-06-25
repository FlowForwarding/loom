if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.connectionURL = "ws://" + location.host + "/clientsock.yaws";
//NCI.connectionURL = "ws://10.48.2.81:28080/clientsock.yaws";
NCI.limits = {};
NCI.loading = $(".loading");

if (typeof NCI.Connection === 'undefined')
   NCI.Connection = {};
   
if (typeof NCI.Admin === 'undefined')
    NCI.Admin = {}; 
	
NCI.Admin.fillLimits = function(limits){
	NCI.limits = limits;
	$("#nciMaxEdges").text(limits.max_edges);
	$("#nciMaxVertices").text(limits.max_vertices);
	$("#nciMaxCommunities").text(limits.max_communities);
	$("#nciCommunitySizeLimit").text(limits.comm_size_limit);
};  

NCI.processInputLimit = function(value, initial){
	var val = parseInt(value);
	return val != val ? initial : val;
}

$("#saveLimits").on("click", function(){
	if ($("#newMaxEdges").data("invalid") !== undefined  ||
	 $("#newMaxVertices").data("invalid") !== undefined  ||
     $("#newMaxCommunities").data("invalid") !== undefined  ||
     $("#nciCommunitySizeLimit").data("invalid") !== undefined ){
		 return;
     };
	 var newMaxEdge = NCI.processInputLimit($("#newMaxEdges").val(), NCI.limits.max_edges);
	 var newMaxVertices = NCI.processInputLimit($("#newMaxVertices").val(), NCI.limits.max_vertices);
	 var newMaxCommunities = NCI.processInputLimit($("#newMaxCommunities").val(), NCI.limits.max_communities);
	 var newCommunitySizeLimit = NCI.processInputLimit($("#newCommunitySizeLimit").val(), NCI.limits.comm_size_limit);
	 NCI.Connection.setLimits(newMaxVertices, newMaxEdge, newCommunitySizeLimit, newMaxCommunities);
});
   
NCI.Connection.onmessage  = function (e) {
   	var data = eval("tmp = " + e.data ); 
	switch(data.action) {
	    case "getlimits":
			NCI.Admin.fillLimits(data.limits);
	        break;
	    case "setlimits":
			NCI.Admin.fillLimits(data.limits);
	       // NCI.socialGraph.show(true, false, false, false);
	        break;				
	    default:
			break;
	};  
	NCI.loading.hide();
};
   
NCI.Connection.getLimits = function(){
	NCI.loading.show();
    NCI.Socket.send('{"action":"getlimits","Time": "' + new Date() + '"}');
};

NCI.Connection.setLimits = function(max_vertices, max_edges, comm_size_limit, max_communities){
	NCI.loading.show();
	//max_verticesconsole.log(max_vertices);
	 NCI.Socket.send('{"action":"setlimits","limits": {"max_vertices": ' + max_vertices + ',' +
	 '"max_edges":' + max_edges + ',' +
	 '"comm_size_limit":' + comm_size_limit + ',' +
	 '"max_communities":' + max_communities + '}}');
}

NCI.initSocket = function(){
	NCI.Socket = new WebSocket(NCI.connectionURL);
	NCI.Socket.onerror = function (e) {
		//NCI.chartData = [];
		//$(".disconected").show();
	};
	NCI.Socket.onclose = function (e) {
		$(".disconected").show();
	};
	NCI.Socket.onopen = function () {
		$(".disconected").hide();
		NCI.Connection.getLimits();
	};
	NCI.Socket.onmessage  = function (e) {
		NCI.Connection.onmessage(e);
	};
};

NCI.initSocket();
