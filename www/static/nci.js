if (typeof NCI === 'undefined')
   NCI = {};
 
NCI.label = {}; 
NCI.label.sec = 'sec';   
NCI.label.min = 'min'; 
NCI.label.hours = 'hrs'; 
NCI.label.days = 'day'; 
NCI.label.months = 'mth'; 
NCI.label.year = 'years';

NCI.nciLatestValue = $('#nciLatestValue');
NCI.nepLatestValue = $('#nepLatestValue');
NCI.qpsLatestValue = $('#qpsLatestValue');
NCI.lastUpdateTime = $('#lastUpdateTime');

NCI.ifMobile = function(){
	return /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(navigator.userAgent);
};

NCI.currentNCI = 0;

NCI.setNciLatestValue = function (val, time) {
	
	var colorClass = val > NCI.currentNCI ? 'green' : val == NCI.currentNCI ? 'black' : 'red';
	NCI.currentNCI = val;
	var newVal = NCI.parceNumberForView(val);
	NCI.nciLatestValue.html('<val class="' + colorClass + '"> ' + newVal + ' </val><br><i>updated &nbsp;' + time + '</i> ');
	NCI.lastUpdateTime.html('updated &nbsp;' + time);
};

NCI.setNepLatestValue = function (newVal, time) {
	NCI.nepLatestValue.html('<val>' + newVal + '</val><br><i>updated &nbsp;' + time + '</i>');
	NCI.lastUpdateTime.html('updated &nbsp;' + time);
};

NCI.setQpsLatestValue = function (newVal, time) {
	NCI.qpsLatestValue.html('<val>' + newVal + '</val> <br><i>updated &nbsp;' + time + '</i>');
	NCI.lastUpdateTime.html('updated &nbsp;' + time);
};

NCI.convertDateForServer = function(date){
	//we need to get such format in UTC 2013-10-27T13:11:39Z for server
	var returnDate = new Date(date - NCI.time_adjustment).toISOString();
	returnDate = returnDate.substring(0, returnDate.length - 5) + "Z";
	return returnDate;
};

NCI.getMillisecondsBefore = function(num, dimention){
	var millisecondsBefore = 0;
	switch (dimention)
	{
	case NCI.label.min:
		millisecondsBefore =  num * 60;
		break;
	case NCI.label.hours:
		millisecondsBefore =  num * 60 * 60;
		break;
	case NCI.label.days:
		millisecondsBefore =  num * 60 * 60 * 24;
		break;
	case NCI.label.months:
		millisecondsBefore =  num * 60 * 60 * 24 * 30;
		break;
	case NCI.label.year:
		millisecondsBefore =  num * 60 * 60 * 24 * 30 * 12;
		break;
	}; 
	return millisecondsBefore * 1000;
}

NCI.convertNCITimePeriodToDate = function(num, dimention){

	return new Date(new Date - NCI.getMillisecondsBefore(num, dimention));
};

NCI.parceDateForLastUpdate = function(stringDate){
	var date = new Date(stringDate)
	var showDate = date.getMonth() + '.' + date.getDate() + '.' + date.getFullYear() % 100
		+ '  ' + date.getHours() + ':' + date.getMinutes() + ':' + date.getSeconds();
	return showDate;
};

NCI.parceDateWithDimention = function(stringDate, dimention){
	var date = new Date(stringDate);
	var uiDate;
	switch (dimention)
	{
	case NCI.label.sec:
		uiDate = date.getMinutes() + "m" + date.getSeconds() + "s";
		break;
	case NCI.label.min:
		uiDate =  date.getHours() + "h" + date.getMinutes() + "m" ;
		break;
	case NCI.label.hours:
		uiDate =  NCI.getWeekDay(date.getDay()) + " " + date.getMinutes() + "h" ;
		break;
	case NCI.label.days:
		uiDate =  NCI.getMonthName(date.getMonth()) + " " + date.getDate();
		break;
	case NCI.label.months:
		uiDate =  NCI.getMonthName(date.getMonth()) + "'" + date.getYear();
		break;
	};
	return uiDate;
};

NCI.getMonthName = function(mnth){
	switch(mnth)
	{
	case 0:
		return "Jan";
	case 1:
		return "Feb";
	case 2:
		return "Mar";
	case 3:
		return "Apr";
	case 4:
		return "May";
	case 5:
		return "Jun";
	case 6:
		return "Jul";				
	case 7:
		return "Aug";
	case 8:
		return "Sep";
	case 9:
		return "Oct";
	case 10:
		return "Nov";
	case 11:
		return "Dec";						
	}
};	

NCI.getWeekDay = function(day){
	switch(day)
	{
	case 0:
		return "Sun";
	case 1:
		return "Mon";
	case 2:
		return "Tue";
	case 3:
		return "Wed";
	case 4:
		return "Thu";
	case 5:
		return "Fri";
	case 6:
		return "Sat";					
	}
};

NCI.parceNumberForView = function(labelValue){
    return Math.abs(Number(labelValue)) >= 1.0e+9

         ? Math.abs(Number(labelValue)) / 1.0e+9.toFixed(1) + " B"
         : Math.abs(Number(labelValue)) >= 1.0e+6

         ? (labelValue/ 1.0e+6).toFixed(1)  + " M"
         : Math.abs(Number(labelValue)) >= 1.0e+3

         ? (labelValue / 1.0e+3).toFixed(1) + " K"
         : Math.abs(Number(labelValue));
};

NCI.periodLabel = $('#periodLabel');

NCI.slider = (function(){
	var me =  $('#slider');
	
	function genMinXScale(val){
		return {val : val, dim: NCI.label.min, pointsNum: 61, indexDim: NCI.label.sec}
	};
	
	function genDecMinXScale(val){
		return {val : val*10, dim: NCI.label.min, pointsNum: 11, indexDim: NCI.label.min}
	};
	
	function genHourXScale(val){
		return {val : val, dim: NCI.label.hours, pointsNum: 11, indexDim: NCI.label.min}
	};
	
	function genDecHourXScale(val){
		return {val : val*10, dim: NCI.label.hours, pointsNum: 11, indexDim: NCI.label.hours}
	};
	
	function genDayXScale(val){
		return {val : val, dim: NCI.label.days, pointsNum: 13, indexDim: NCI.label.hours}
	};
	
	function genDecDayXScale(val){
		return {val : val*10, dim: NCI.label.days, pointsNum: 11, indexDim: NCI.label.days}
	};
	
	function genMonthXScale(val){
		return {val : val, dim: NCI.label.months, pointsNum: 11, indexDim: NCI.label.days}
	};
	
	function genYearXScale(val){
		return {val : val, dim: NCI.label.year, pointsNum: 13, indexDim: NCI.label.months}
	};

	me.xAxesScale = [];
	
	for (var i = 1; i< 10; i++){
		me.xAxesScale.push(genMinXScale(i));
	};
	
	for (var i = 1; i< 6; i++){
		me.xAxesScale.push(genDecMinXScale(i));
	};
	
	for (var i = 1; i< 10; i++){
		me.xAxesScale.push(genHourXScale(i));
	};
	
	for (var i = 1; i< 3; i++){
		me.xAxesScale.push(genDecHourXScale(i));
	};
	
	for (var i = 1; i< 10; i++){
		me.xAxesScale.push(genDayXScale(i));
	};
	
	for (var i = 1; i< 4; i++){
		me.xAxesScale.push(genDecDayXScale(i));
	};
	
	for (var i = 1; i< 12; i++){
		me.xAxesScale.push(genMonthXScale(i));
	};
	
	for (var i = 1; i< 11; i++){
		me.xAxesScale.push(genYearXScale(i));
	};
	
	me[0].max = me.xAxesScale.length-1;
	
	var getValueByRange = function(intValue){
		var date, friquent;
		var xScaleVal = me.xAxesScale[intValue];
		date = xScaleVal.val + " " + xScaleVal.dim;
		return {date : date, friquent: friquent};
    };
	
	me.on('change', function(){
		me.updateValueLabel();
	});
	
	me.on('mouseup', function(){
		me.updateChart();
	});
	
	me.updateChart = function (){
		var xScaleVal = me.xAxesScale[me[0].value];
		NCI.gapForChartUpdate = NCI.getMillisecondsBefore(xScaleVal.val, xScaleVal.dim)/ (xScaleVal.pointsNum -1);
		var endDate = NCI.convertNCITimePeriodToDate(xScaleVal.val, xScaleVal.dim)
	    NCI.Connection.send('{"request":"more_data","start": "' + NCI.convertDateForServer(endDate) + '",' +
		     '"end": "' + NCI.convertDateForServer(new Date()) + '","max_items": "' + xScaleVal.pointsNum + '"}');
		me.updateValueLabel();
	}

	me.updateValueLabel = function(){
		NCI.periodLabel.html("<small> data for last </small> " + getValueByRange(parseInt(me[0].value)).date)
	};
	return me;
}());	

$('.slider .icon-plus-sign').on('click', function(){
	var curVal = NCI.slider[0].value;
	if (curVal < parseInt(NCI.slider[0].max)){
		NCI.slider[0].value = 1 + parseInt(NCI.slider[0].value);
		NCI.slider.updateChart();
	};
});

$('.slider .icon-minus-sign').on('click', function(){
	var curVal = NCI.slider[0].value;
	if (curVal > 0){
	   NCI.slider[0].value = NCI.slider[0].value - 1;
	   NCI.slider.updateChart();
    };
});

$('.round-info').on('touchend', function(){
	$('.round-info').trigger('click');
})

