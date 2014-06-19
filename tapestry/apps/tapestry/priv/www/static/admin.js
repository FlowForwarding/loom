if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.connectionURL = "ws://" + location.host + "/clientsock.yaws";
//NCI.connectionURL = "ws://10.48.2.81:28080/clientsock.yaws";

if (typeof NCI.Connection === 'undefined')
   NCI.Connection = {};
   
if (typeof NCI.Admin === 'undefined')
    NCI.Admin = {}; 
	
NCI.Admin.fillLimits = function(limits){
	$("#nciMaxEdges").text(limits.max_edges);
	$("#nciMaxVertices").text(limits.max_vertices);
	$("#nciMaxCommunities").text(limits.max_communities);
	$("#nciCommunitySizeLimit").text(limits.comm_size_limit);
};  

$("#saveLimits").on("click", function(){
	if ($("#newMaxEdges").data("invalid") !== undefined  ||
	 $("#newMaxVertices").data("invalid") !== undefined  ||
     $("#newMaxCommunities").data("invalid") !== undefined  ||
     $("#nciCommunitySizeLimit").data("invalid") !== undefined ){
		 return;
     };
	 NCI.Connection.setLimits($("#newMaxEdges").val(), 
	    $("#newMaxVertices").val(),
		$("#newMaxEdges").val(),
		$("#newMaxCommunities").val(),
		$("#nciCommunitySizeLimit").val());

	console.log($("#newMaxEdges").val());
});
   
NCI.Connection.onmessage  = function (e) {
   	var data = eval("tmp = " + e.data ); 
	switch(data.action) {
	    case "getlimits":
			NCI.Admin.fillLimits(data.limits);
	        break;
	    case "setimits":
			NCI.Admin.fillLimits(data.limits);
	       // NCI.socialGraph.show(true, false, false, false);
	        break;				
	    default:
			return;
	};  
};
   
NCI.Connection.getLimits = function(){
    NCI.Socket.send('{"action":"getlimits","Time": "' + new Date() + '"}');
};

NCI.Connection.setLimits = function(max_vertices, max_edges, comm_size_limit, max_communities){
	//max_verticesconsole.log(max_vertices);
	 NCI.Socket.send('{"action":"setlimits","limits": {"max_vertices": ' + max_vertices + ',' +
	 '"max_edges": 9000,' +
	 '"comm_size_limit": 100,' +
	 '"max_communities": 300 }}');
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
