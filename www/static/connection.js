if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.start_time; // no data exists on the server before
NCI.time_adjustment = 0; //difference between client and server time in milliseconds
NCI.lastChartDate = 0;   
NCI.gapForChartUpdate = 0;   //10 sec
   
NCI.Connection = new WebSocket("ws://" + 'nci.ilabs.inca.infoblox.com:28080' + "/clientsock.yaws");
NCI.Connection.onopen = function () {
	NCI.Connection.send('START_DATA');
};

NCI.Connection.onmessage  = function (e) {
	var data = eval("tmp = " + e.data );
	console.log(e.data);
	if (data.start_time){
	    NCI.time_adjustment = new Date() - new Date(data.current_time);
		return;
	};
	if (!NCI.lastChartDate){
		var xScaleVal = NCI.slider.xAxesScale[NCI.slider [0].value];
		NCI.gapForChartUpdate = NCI.getMillisecondsBefore(xScaleVal.val, xScaleVal.dim)/ (xScaleVal.pointsNum);
	};
	
	if (e.data.length < 60){
		var dateVal = new Date(data.Time);
		if (data.NCI){
			NCI.setNciLatestValue(data.NCI, NCI.parceDateForLastUpdate(data.Time));
			if (dateVal - NCI.lastChartDate > NCI.gapForChartUpdate){ 
				var params = {};
				params.time = dateVal;
				params.NCI = data.NCI;
				NCI.lastChartDate = dateVal;
				NCI.addValueToChart(params);
			};
		};
		data.QPS = 0;
		if (data.QPS !== undefined)
			NCI.setQpsLatestValue(NCI.parceNumberForView(data.QPS), NCI.parceDateForLastUpdate(data.Time));
		if (data.NEP !== undefined)
			NCI.setNepLatestValue(NCI.parceNumberForView(data.NEP), NCI.parceDateForLastUpdate(data.Time));
	} else {
		NCI.chartData = [];
		//we recieve such format:
		// {"Time":"2013-10-27T13:01:09Z","NCI":99,
		// "Time":"2013-10-27T13:11:39Z","NCI":8,
		// "Time":"2013-10-27T13:22:15Z","NCI":18,
		// "Time":"2013-10-27T13:33:01Z","NCI":87} 
		var recievedDataArray = e.data.substring(1, e.data.length - 1).split(',');
		for (var i = 0; i < recievedDataArray.length/2; i++){
			var curIndex = 2 * i;
			var timeValue = recievedDataArray[curIndex].substring(7);
			timeValue = timeValue.substring(1, timeValue.length - 1);
			var nciValue = recievedDataArray[curIndex + 1];
			nciValue = parseInt(nciValue.split(":")[1]);
			NCI.chartData.push([new Date(timeValue), nciValue]);
		};
		
		var timeValue = recievedDataArray[recievedDataArray.length/2].substring(7);
		timeValue = timeValue.substring(1, timeValue.length - 1);
		NCI.lastChartDate = new Date(timeValue);	
		
	    NCI.chart.series[0].data = NCI.chartData;
		NCI.chart.resetAxesScale();
	    NCI.chart.replot( {data: [ NCI.chartData]});
	};
	
};

NCI.Connection.onerror= function (e) {
	console.log('error ' + e.data);
};

NCI.Connection.onclose = function (e) {
	console.log('close ' + e.data);
};