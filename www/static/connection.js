if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.start_time; // no data exists on the server before
NCI.time_adjustment = 0; //difference between client and server time in milliseconds
   
NCI.Connection = new WebSocket("ws://" + 'nci.ilabs.inca.infoblox.com:28080' + "/clientsock.yaws");
NCI.Connection.onopen = function () {
	console.log('opened');
	NCI.Connection.send('START_DATA');
};

NCI.Connection.onmessage  = function (e) {
	var data = eval("tmp = " + e.data );
	if (data.start_time){
	    NCI.time_adjustment = new Date() - new Date(data.current_time);
		return;
	};
	
	if (NCI.slider[0].value == 0){
		console.log(data);
		var params = {};
		params.time = new Date(data.Time).getMinutes() + ":" + new Date(data.Time).getSeconds();
		if (data.NCI){
			NCI.setNciLatestValue(NCI.parceNumberForView(data.NCI), NCI.parceDateForLastUpdate(data.Time));
			params.NCI = data.NCI;
			NCI.addValueToChart(params);
		};
		if (data.QPS)
			NCI.setQpsLatestValue(NCI.parceNumberForView(data.QPS), NCI.parceDateForLastUpdate(data.Time));
		if (data.NEP)
			NCI.setNepLatestValue(NCI.parceNumberForView(data.NEP), NCI.parceDateForLastUpdate(data.Time));
	} else if (e.data.length > 60){
		NCI.chartData = [];
		//we recieve such format:
		// {"Time":"2013-10-27T13:01:09Z","NCI":99,
		// "Time":"2013-10-27T13:11:39Z","NCI":8,
		// "Time":"2013-10-27T13:22:15Z","NCI":18,
		// "Time":"2013-10-27T13:33:01Z","NCI":87} 
		var dimention = NCI.slider.xAxesScale[NCI.slider[0].value].indexDim;
		var recievedDataArray = e.data.substring(1, e.data.length - 1).split(',');
		var chartDataArray = [];
		for (var i = 0; i < recievedDataArray.length/2; i++){
			var curIndex = 2 * i;
			var timeValue = recievedDataArray[curIndex].substring(7);
			timeValue = timeValue.substring(1, timeValue.length - 1);
			var nciValue = recievedDataArray[curIndex + 1];
			nciValue = parseInt(nciValue.split(":")[1]);
			chartDataArray.push([NCI.parceDateWithDimention(timeValue, dimention), nciValue]);
		};
	    NCI.chart.series[0].data = chartDataArray;
		NCI.chart.resetAxesScale();
	    NCI.chart.replot( {data: [ chartDataArray]});
	};
	
};

NCI.Connection.onerror= function (e) {
	console.log('error ' + e.data);
};

NCI.Connection.onclose = function (e) {
	console.log('close ' + e.data);
};