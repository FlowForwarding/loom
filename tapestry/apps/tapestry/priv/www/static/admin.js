if (typeof NCI === 'undefined')
   NCI = {};
  
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
     $("#newCommunitySizeLimit").data("invalid") !== undefined ){
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
	        break;				
	    default:
			break;
	};  
	NCI.loading.hide();
};


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
