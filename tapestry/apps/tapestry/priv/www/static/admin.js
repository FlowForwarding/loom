if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.connectionURL = "ws://" + location.host + "/clientsock.yaws";
NCI.connectionURL = "ws://10.48.2.81:28080/clientsock.yaws";

if (typeof NCI.Connection === 'undefined')
   NCI.Connection = {};
   
NCI.Connection.onmessage  = function (e) {
   	var data = eval("tmp = " + e.data ); 
	switch(data.action) {
	    case "getlimits":
			$("#nciMaxEdges").text(data.limits.max_edges);
			$("#nciMaxVertices").text(data.limits.max_vertices);
			$("#nciMaxCommunities").text(data.limits.max_communities);
			$("#nciCommunitySizeLimit").text(data.limits.comm_size_limit);
	        break;
	    case "setimits":
			console.log(data);
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
	 NCI.Socket.send('{"action":"setimits","max_vertices": "' + max_vertices + '",' +
	 '"max_edges": "' + max_edges + '",' +
	 '"comm_size_limit": "' + comm_size_limit + '",' +
	 '"max_communities": "' + max_communities + '"' +
	 '}');
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
