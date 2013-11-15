// To switch on emulation mode  - in nci.html add string
// <script src='static/server_emulator.js'></script>
// after
// <script src='static/chart.js'></script>

if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.Emulator = {};  
NCI.Emulator.liveDataFrequency = 500; //in mseconds
NCI.Emulator.dataAvailablePeriod = NCI.chartPeriods.onemnth;  //in mseconds
NCI.Emulator.serverResponceDelay = 3000;  //in mseconds

NCI.Emulator.startData = new Date();

NCI.Emulator.liveNCIData = function(){
	// server returns 
	// {"Time":"2013-11-04T12:54:41Z","NCI":5}
	// {"Time":"2013-11-04T12:54:41Z","NEP":87}
	// {"Time":"2013-11-04T12:54:16Z","QPS":16.451612903225808} 
	var event = {};
	event.data = JSON.stringify({Time:  NCI.convertDateForServer(new Date()), NCI: Math.floor((Math.random()*3)+5)});
	NCI.Connection.onmessage(event);
};

//overriders   
NCI.Connection.startData = function() {
	NCI.Emulator.startData = new Date();
	var event = {};
	// server returns {"start_time":"2013-11-03T17:37:31Z","current_time":"2013-11-04T12:54:42Z"} 
	event.data = JSON.stringify({start_time: new Date() - NCI.Emulator.dataAvailablePeriod, current_time: new Date()});
	NCI.Connection.onmessage(event);
	setInterval(NCI.Emulator.liveNCIData, NCI.Emulator.liveDataFrequency) 
}; 

NCI.Connection.moreData = function(startTime, endTime, pointsNum) {
	// server returns smth like this
	// {"Time":"2013-11-03T17:37:31Z","NCI":2,
	// "Time":"2013-11-03T18:37:30Z","NCI":4,
	// .......
	// "Time":"2013-11-04T12:32:20Z","NCI":5,
	// "Time":"2013-11-04T13:33:13Z","NCI":5} 
	
	if (NCI.Emulator.startData - NCI.Emulator.dataAvailablePeriod > startTime){
		startTime = NCI.Emulator.startData - NCI.Emulator.dataAvailablePeriod;
	};
	
    var event = {};
	event.data = '{';
	var dateGap = (endTime - startTime) / pointsNum;
	if (startTime < endTime) {
		for (var i=0; i <= pointsNum; i++ ){
			event.data += '"Time":"' +  NCI.convertDateForServer(startTime + dateGap*i) 
			+ '","NCI":' + Math.floor((Math.random()*3)+5) +',';
		};
		event.data = event.data.replace(/,$/,'}');
		setTimeout(NCI.Connection.onmessage, NCI.Emulator.serverResponceDelay, event);
	};
	
};   

//Override to do nothing, for the case if connection opened successfull in emulation mode
NCI.Connection.onopen = function () {
	
};

NCI.Connection.startData();
   