if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.start_time; // no data exists on the server before
NCI.time_adjustment = 0; //difference between client and server time in milliseconds
NCI.numOfPoints = 200;
//NCI.Connection = new WebSocket("ws://epamove.herokuapp.com");
NCI.connectionURL = "ws://" + location.host + "/clientsock.yaws";
NCI.connectionURL = "ws://10.32.3.207:28080/clientsock.yaws";

NCI.Connection = [];

NCI.lastUpdateTimeVal = new Date();
NCI.lastRedrawTimeVal = new Date();

NCI.Connection.onmessage  = function (e) {
	var data = eval("tmp = " + e.data );

	if (data.start_time){
		// to show utc time
		// NCI.time_adjustment = new Date() - new Date(data.current_time) - new Date().getTimezoneOffset()*1000*60;
		var curTime = new Date(data.current_time);
		NCI.time_adjustment = new Date() - curTime;
		if (!NCI.chart){
			NCI.start_time = new Date(data.start_time);
			NCI.zoomLinks.setTimePeriod(curTime - NCI.start_time);
			NCI.initChart(curTime - NCI.time_adjustment);
			
			NCI.curChartPeriod = (NCI.chartPeriods.twoyears - (curTime - NCI.start_time) < 0) ? NCI.chartPeriods.twoyears : (curTime - NCI.start_time);
			NCI.Connection.moreData(new Date() - NCI.curChartPeriod, new Date(), NCI.numOfPoints);
		};
		return;
	} else if (data.action == "collectors") {
		//data.Time
		 NCI.collectorsTable.fillData(data.Collectors);
	} else if (data.action == "QPS") {
			NCI.setQpsLatestValue(NCI.parceNumberForView(data.QPS, 1), NCI.parceDateForLastUpdate(data.Time));
	} else if (data.action == "NEP") {
			NCI.setNepLatestValue(NCI.parceNumberForView(data.NEP), NCI.parceDateForLastUpdate(data.Time));
			NCI.setFlowsLatestValue(NCI.parceNumberForView(data.NE), NCI.parceDateForLastUpdate(data.Time))
	} else if (data.action == "Collectors")	{
		    NCI.setCollectorsLatestValue(NCI.parceNumberForView(data.COLLECTORS), NCI.parceDateForLastUpdate(data.Time));	 
	} else if (data.action == "NCI"){
		var dateVal = new Date(data.Time);
		if (data.NCI){
			NCI.lastUpdateTimeVal = dateVal;
			NCI.nciUpdateDateServer = data.Time;
			NCI.setNciLatestValue(data.NCI, NCI.parceDateForLastUpdate(data.Time), data.activities);
			if (!NCI.chart){
				 NCI.initChart(new Date(data.Time) - NCI.time_adjustment);
			} else {
				//update chart not faster then once in 3 seconds and only for case when we are in 2 years mode
				if (new Date() - NCI.lastRedrawTimeVal < 3000  && NCI.curChartPeriod <= NCI.chartPeriods.twoyears )
					return;

				NCI.chartData.push([new Date(dateVal - NCI.time_adjustment).getTime(), data.NCI]);
				NCI.lastRedrawTimeVal = new Date();				
				//check if chart selected area right border is in the beginning of chart ,
				//then automaticaly move selection area to the beginning (error ~ one minute)
				if (parseInt(NCI.chart.xAxisExtremes()[1] - NCI.chart.xAxisRange()[1]) < NCI.chartPeriods.minute && NCI.chart &&  NCI.chart.dateWindow_){
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
		} ;
	} else if (data.action == "NCIDetails"){
		NCI.setupCommunities(data)
		$('#nciDetailsTabs').find("a")[4].click();
	} else {
		//we recieve such format:
		// [{"Time":"2013-10-27T13:01:09Z","NCI":99},
		// {"Time":"2013-10-27T13:11:39Z","NCI":8},
		// {"Time":"2013-10-27T13:22:15Z","NCI":18},
		// {"Time":"2013-10-27T13:33:01Z","NCI":87}]
		var newData = [];
		for (var i = 0; i < data.length; i++){
			var dataItem = data[i];
			newData.push([new Date(new Date(dataItem.Time) - NCI.time_adjustment).getTime() , dataItem.NCI]);
		};
		
		NCI.setBottomChartDates(new Date(new Date() - NCI.time_adjustment).getTime() - newData[0][0]);
		
		NCI.detailedChartPeriod = NCI.detailedChartPeriod ? NCI.detailedChartPeriod : NCI.curChartPeriod/10;
		if (NCI.detailedChartPeriod > (new Date().getTime() - newData[0][0])){
			NCI.detailedChartPeriod = new Date().getTime() - newData[0][0];
		};	
		
		var ranges = NCI.DetectRangesForPeiod( NCI.detailedChartPeriod, newData);
		newData.push([new Date(new Date() - NCI.time_adjustment).getTime() , null]);
		NCI.chartData = newData;
		
	 	NCI.chart.updateOptions({
			dateWindow: [ranges[0] - NCI.time_adjustment, ranges[1] - NCI.time_adjustment],
			connectSeparatedPoints: true,
			file: NCI.chartData
	 	});
	};
	
};

NCI.DetectRangesForPeiod = function(detailedChartPeriod, chartData){
	if (chartData.length < 2)
		return [0,0];
	var endRangeDate = chartData[chartData.length - 1][0];
	if (!chartData[chartData.length - 1][1])
		endRangeDate = chartData[chartData.length - 2][0];
	var firstDate = chartData[0][0];
	if (endRangeDate - detailedChartPeriod < firstDate){
		var startRangeDate = firstDate;
		if (startRangeDate +  detailedChartPeriod < new Date().getTime()){
			endRangeDate = startRangeDate +  detailedChartPeriod;
		} else {
			endRangeDate = new Date().getTime();
		};
	} else {
		var startRangeDate = endRangeDate - detailedChartPeriod;
	};
	return [startRangeDate, new Date().getTime()];
};

NCI.Connection.NCIDetails = function(time){
    NCI.Socket.send('{"action":"NCIDetails","Time": "' + time + '"}');
};

NCI.Connection.CollectorsDetails = function(time){
	 NCI.Socket.send('{"action":"collectors","Time": "' + time + '"}');
}

NCI.Connection.startData = function() {
	NCI.Socket.send('START_DATA');
};

NCI.Connection.moreData = function(startTime, endTime, pointsNum) {
	startTime = NCI.convertDateForServer(startTime);
	endTime = NCI.convertDateForServer(endTime);
    NCI.Socket.send('{"request":"more_data","start": "' + 
		startTime + '",' +  '"end": "' + endTime + '","max_items": "' + pointsNum + '"}');
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
		NCI.Connection.startData();
	};
	NCI.Socket.onmessage  = function (e) {
		NCI.Connection.onmessage(e);
	};
};

NCI.initSocket();

$(".disconected img").on('click', function(){
	$(".disconected").hide();
	NCI.initSocket();
});
