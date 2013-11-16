if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.start_time; // no data exists on the server before
NCI.time_adjustment = 0; //difference between client and server time in milliseconds
NCI.numOfPoints = 200;

NCI.Connection = new WebSocket("ws://"  + "nci.ilabs.inca.infoblox.com:28080" + "/clientsock.yaws");
NCI.Connection.onopen = function () {
	NCI.Connection.startData();
};

NCI.lastUpdateTimeVal = new Date();
NCI.lastRedrawTimeVal = new Date();

NCI.Connection.onmessage  = function (e) {
	var data = eval("tmp = " + e.data );
	//console.log(e.data);
	if (data.start_time){
		NCI.time_adjustment = new Date() - new Date(data.current_time) - new Date().getTimezoneOffset()*1000*60;
		if (!NCI.chart){
			NCI.initChart(new Date(data.current_time) - NCI.time_adjustment);
			NCI.Connection.moreData(new Date() - NCI.curChartPeriod, new Date(), NCI.numOfPoints);
		};
		return;
	};
	
	if (e.data.length < 60){
		var dateVal = new Date(data.Time);
		if (data.NCI){
			NCI.lastUpdateTimeVal = dateVal;
			NCI.setNciLatestValue(data.NCI, NCI.parceDateForLastUpdate(data.Time));
			if (!NCI.chart){
				 NCI.initChart(new Date(data.Time) - NCI.time_adjustment);
			} else {
				//next expression check that minimum required time passed from last time graph was redrawed
				//for half month period (current chart period) - is 3 seconds, for month - 6 seconds and then encreases linerar
				if (new Date() - NCI.lastRedrawTimeVal < NCI.curChartPeriod/NCI.chartPeriods.halfmnth*3000)
					return;

				NCI.chartData.push([new Date(dateVal - NCI.time_adjustment).getTime(), data.NCI]);
				NCI.lastRedrawTimeVal = new Date();
				//next cycle checks if there are values in chart data set, 
				//that are older then current chart time period
				var len = NCI.chartData.length - 2;
				for (var ind = 0; ind < len; ind++ ){
					if (new Date(data.Time).getTime() - NCI.chartData[ind + 1][0] > NCI.curChartPeriod){
						NCI.chartData.shift();
					} else {
						break;
					}
				}
				
				//check if chart selected area right border is in the beginning of chart ,
				//then automaticaly move selection area to the beginning (error ~ one minute)
				if (parseInt(NCI.chart.xAxisExtremes()[1] - NCI.chart.xAxisRange()[1]) < NCI.chartPeriods.minute && NCI.chart){
					var diff = NCI.chart.dateWindow_[1] - NCI.chart.dateWindow_[0];
					NCI.chart.updateOptions({
						file: NCI.chartData,
						dateWindow: [new Date(dateVal - diff  - NCI.time_adjustment).getTime(),  
							new Date(dateVal  - NCI.time_adjustment).getTime()]
					});
				} else {
					NCI.chart.updateOptions({
						file: NCI.chartData
					});
				}
			};
		};

		if (data.QPS !== undefined)
			NCI.setQpsLatestValue(NCI.parceNumberForView(data.QPS, 1), NCI.parceDateForLastUpdate(data.Time));
		if (data.NEP !== undefined)
			NCI.setNepLatestValue(NCI.parceNumberForView(data.NEP), NCI.parceDateForLastUpdate(data.Time));
	} else {
		//we recieve such format:
		// {"Time":"2013-10-27T13:01:09Z","NCI":99,
		// "Time":"2013-10-27T13:11:39Z","NCI":8,
		// "Time":"2013-10-27T13:22:15Z","NCI":18,
		// "Time":"2013-10-27T13:33:01Z","NCI":87} 
		var newData = [[new Date(new Date() - NCI.time_adjustment - NCI.curChartPeriod).getTime(), 0]];
		var recievedDataArray = e.data.substring(1, e.data.length - 1).split(',');
		for (var i = 0; i < recievedDataArray.length/2; i++){
			var curIndex = 2 * i;
			var timeValue = recievedDataArray[curIndex].substring(7);
			timeValue = timeValue.substring(1, timeValue.length - 1);
			var nciValue = recievedDataArray[curIndex + 1];
			nciValue = parseInt(nciValue.split(":")[1]);
			newData.push([new Date(new Date(timeValue) - NCI.time_adjustment).getTime() , nciValue]);
		};
		newData.push([new Date(new Date() - NCI.time_adjustment).getTime() , null]);
		NCI.chartData = newData;
	 	NCI.chart.updateOptions({
			connectSeparatedPoints: true,
			file: NCI.chartData
	 	});
	};
	
};

NCI.Connection.startData = function() {
	NCI.Connection.send('START_DATA');
};

NCI.Connection.moreData = function(startTime, endTime, pointsNum) {
	startTime = NCI.convertDateForServer(startTime);
	endTime = NCI.convertDateForServer(endTime);
    NCI.Connection.send('{"request":"more_data","start": "' + 
		startTime + '",' +  '"end": "' + endTime + '","max_items": "' + pointsNum + '"}');
};

NCI.Connection.onerror = function (e) {
	console.log('error ' + e.data);
};

NCI.Connection.onclose = function (e) {
	console.log('close ' + e.data);
};