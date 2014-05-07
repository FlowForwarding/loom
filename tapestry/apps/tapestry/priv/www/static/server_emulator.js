// To switch on emulation mode  - in nci.html add string
// <script src='static/server_emulator.js'></script>
// after
// <script src='static/chart.js'></script>

if (typeof NCI === 'undefined')
   NCI = {};
   
NCI.Emulator = {};  
NCI.Emulator.liveDataFrequency = 5000; //in mseconds ( 5 second updates)
NCI.Emulator.dataAvailablePeriod = NCI.chartPeriods.twodays;  //in mseconds
//NCI.Emulator.dataAvailableTill = NCI.chartPeriods.sixmnth; //time from now in mseconds - last nci value update
NCI.Emulator.serverResponceDelay = 1500;  //in mseconds

NCI.Emulator.startData = new Date();

NCI.Emulator.liveData = function(){
	// server returns 
	// {"Time":"2013-11-04T12:54:41Z","NCI":5}
	// {"Time":"2013-11-04T12:54:41Z","NEP":87}
	// {"Time":"2013-11-04T12:54:16Z","QPS":16.451612903225808} 
	var event = {};
	var nciValue = Math.floor((Math.random()*3)+5);
	var activities = [];
	var numOfActivities = nciValue + Math.floor((Math.random()*3)+2);
	for (var i=0; i<numOfActivities; i++){
		activities.push({
			name: "Activity " + i + " name", 
			size: i+1, 
			endpoints: i+1,
			details: "activity " + i +  " details"
		});
	};
	event.data = JSON.stringify({Time:  new Date(), 
		NCI: Math.floor((Math.random()*3)+5),
		activities: activities
	});
	NCI.Connection.onmessage(event);
	
	var nepEvent = {};
	nepEvent.data = JSON.stringify({Time:  new Date(), NEP: Math.floor((Math.random()*40)+5)});
	NCI.Connection.onmessage(nepEvent);
	
	var qpsEvent = {};
	qpsEvent.data = JSON.stringify({Time:  new Date(), QPS: Math.floor((Math.random()*40)+5)});
	NCI.Connection.onmessage(qpsEvent);
	
	var collectorEvent = {};
	var collectors = [];
	var numOfCollectors = (Math.random()*30)+1;
	for (var i=0; i<numOfCollectors; i++){
		collectors.push({
			name: "Collector" + i,
			ip: "10.32.3.154",
			nep: (Math.random()*100)+5,
			qps: (Math.random()*1000)+5
		});
	}
	collectorEvent.data = JSON.stringify({Time:  new Date(), COLLECTORS: collectors});
	NCI.Connection.onmessage(collectorEvent);
};

//overriders   
NCI.Connection.startData = function() {
	NCI.Emulator.startData = new Date();
	var event = {};
	// server returns {"start_time":"2013-11-03T17:37:31Z","current_time":"2013-11-04T12:54:42Z"} 
	event.data = JSON.stringify({start_time: new Date(new Date() - NCI.Emulator.dataAvailablePeriod), 
		current_time:  new Date()});
	NCI.Connection.onmessage(event);
	setInterval(NCI.Emulator.liveData, NCI.Emulator.liveDataFrequency) ;
}; 

NCI.Connection.moreData = function(startTime, endTime, pointsNum) {
	// startTime = new Date(startTime);
	// endTime = new Date(endTime);
	// console.log(new Date(startTime));
	// console.log(new Date(endTime));
	// server returns smth like this
	// {"Time":"2013-11-03T17:37:31Z","NCI":2,
	// "Time":"2013-11-03T18:37:30Z","NCI":4,
	// .......
	// "Time":"2013-11-04T12:32:20Z","NCI":5,
	// "Time":"2013-11-04T13:33:13Z","NCI":5} 
	
	if (new Date().getTime() - NCI.Emulator.dataAvailableTill < endTime)
		endTime = new Date().getTime() - NCI.Emulator.dataAvailableTill;
	
	if (NCI.Emulator.startData - NCI.Emulator.dataAvailablePeriod > startTime){
		startTime = NCI.Emulator.startData - NCI.Emulator.dataAvailablePeriod;
	}
	
    var event = {};
	event.data = '{';
	var dateGap = (endTime - startTime) / pointsNum;
	if (startTime < endTime) {
		for (var i=0; i <= pointsNum; i++ ){
			event.data += '"Time":"' +  new Date(startTime + dateGap*i) +
			'","NCI":' + Math.floor((Math.random()*3)+5) +',';
		}
		event.data = event.data.replace(/,$/,'}');
		setTimeout(NCI.Connection.onmessage, NCI.Emulator.serverResponceDelay, event);
	}
	
};   

//Override to do nothing, for the case if connection opened successfull in emulation mode
NCI.Connection.onopen = function () {
	
};

NCI.Connection.startData();
   
