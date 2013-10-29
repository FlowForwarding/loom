if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.start_time; // no data exists on the server before
NCI.time_adjustment = 0; //difference between client and server time in milliseconds
   
NCI.Connection = new WebSocket("ws://" + 'nci.ilabs.inca.infoblox.com:28080' + "/clientsock.yaws");
NCI.Connection.onopen = function () {
	NCI.Connection.send('START_DATA');
};

NCI.Connection.onmessage  = function (e) {
	var data = eval("tmp = " + e.data );
	//console.log(e.data);
	if (data.start_time){
		if (!NCI.chart){
			NCI.initChart(data.current_time);
		};
	    NCI.time_adjustment = new Date() - new Date(data.current_time);
	    NCI.Connection.send('{"request":"more_data","start": "' + NCI.convertDateForServer(new Date() - 1000*60*60*24*30*12*10) + '",' +
		     '"end": "' + NCI.convertDateForServer(new Date()) + '","max_items": "200"}');
		return;
	};
	
	if (e.data.length < 60){
		var dateVal = new Date(data.Time);
		if (data.NCI){
			NCI.setNciLatestValue(data.NCI, NCI.parceDateForLastUpdate(data.Time));
			if (!NCI.chart){
				NCI.initChart(data.Time);
			} else {
				NCI.chartData.push([new Date(dateVal).getTime(), data.NCI]);
				var diff = NCI.chart.dateWindow_[1] - NCI.chart.dateWindow_[0];
				NCI.chart.updateOptions({
					file: NCI.chartData,
					dateWindow: [new Date(dateVal - diff).getTime(),  dateVal.getTime()],
				});
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
			NCI.chartData.push([new Date(timeValue).getMilliseconds(), nciValue]);
		};
		
	    NCI.chart.series[0].data = NCI.chartData;
		NCI.chart.resetAxesScale();
		//g1.updateOptions({file: NCI.chartData});

	   // NCI.chart.replot( {data: [ NCI.chartData]});
	};
	
};

NCI.Connection.onerror= function (e) {
	console.log('error ' + e.data);
};

NCI.Connection.onclose = function (e) {
	console.log('close ' + e.data);
};