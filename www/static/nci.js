if (typeof NCI === 'undefined')
   NCI = {};

NCI.nciLatestValue = $('#nciLatestValue');
NCI.nepLatestValue = $('#nepLatestValue');
NCI.qpsLatestValue = $('#qpsLatestValue');
NCI.lastUpdateTime = $('#lastUpdateTime');

NCI.ifMobile = function(){
	return /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(navigator.userAgent);
}

NCI.setNciLatestValue = function (newVal, time) {
	NCI.nciLatestValue.html('<val> ' + newVal + ' </val> ');
	NCI.lastUpdateTime.html('updated &nbsp;' + time);
};

NCI.setNepLatestValue = function (newVal, time) {
	NCI.nepLatestValue.html('<val>' + newVal + '</val>');
	NCI.lastUpdateTime.html('updated &nbsp;' + time);
};

NCI.setQpsLatestValue = function (newVal, time) {
	NCI.qpsLatestValue.html('<val>' + newVal + '</val> <br>');
	NCI.lastUpdateTime.html('updated &nbsp;' + time);
};

NCI.parceDataForLastUpdate = function(stringDate){
	var date = new Date(stringDate)
	var showDate = date.getMonth() + '.' + date.getDate() + '.' + date.getFullYear() % 100
		+ '  ' + date.getHours() + ':' + date.getMinutes() + ':' + date.getSeconds() ;
	return showDate;
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

NCI.timePeriod = localStorage.timePeriod;

NCI.getChartData = function(){
    $.ajax({
        type: 'GET',
        url: '/chart',
        dataType: 'json',
		data: {
			timePeriod: NCI.timePeriod,
			updateInterval: NCI.updateInterval
		},
        success: function(data){
			//update ui
        },
        error: function(xhr, type){
			console.log('server log');
        }
    });
};

NCI.periodLabel = $('#periodLabel');

NCI.slider = (function(){
	var me =  $('#slider');
	
	function genMinXScale(val){
		return {val : val, dim: 'min', pointsNum: 11, indexMaxVal: 66* val, indexDim: 'sec'}
	};
	
	function genDecMinXScale(val){
		return {val : val*10, dim: 'min', pointsNum: 11, indexMaxVal: 11* val, indexDim: 'min'}
	};
	
	function genHourXScale(val){
		return {val : val, dim: 'hour', pointsNum: 11, indexMaxVal: 66* val, indexDim: 'min'}
	};
	
	function genDecHourXScale(val){
		return {val : val*10, dim: 'hour', pointsNum: 11, indexMaxVal: 11* val, indexDim: 'hours'}
	};
	
	function genDayXScale(val){
		return {val : val, dim: 'day', pointsNum: 13, indexMaxVal: 13* val, indexDim: 'hours'}
	};
	
	function genDecDayXScale(val){
		return {val : val*10, dim: 'day', pointsNum: 11, indexMaxVal: 11* val, indexDim: 'days'}
	};
	
	function genMonthXScale(val){
		return {val : val, dim: 'month', pointsNum: 11, indexMaxVal: 33* val, indexDim: 'day'}
	};
	
	function genYearXScale(val){
		return {val : val, dim: 'year', pointsNum: 13, indexMaxVal: 13* val, indexDim: 'month'}
	};

	var xAxesScale = [];
	
	for (var i = 1; i< 10; i++){
		xAxesScale.push(genMinXScale(i));
	};
	
	for (var i = 1; i< 6; i++){
		xAxesScale.push(genDecMinXScale(i));
	};
	
	for (var i = 1; i< 10; i++){
		xAxesScale.push(genHourXScale(i));
	};
	
	for (var i = 1; i< 3; i++){
		xAxesScale.push(genDecHourXScale(i));
	};
	
	for (var i = 1; i< 10; i++){
		xAxesScale.push(genDayXScale(i));
	};
	
	for (var i = 1; i< 4; i++){
		xAxesScale.push(genDecDayXScale(i));
	};
	
	for (var i = 1; i< 12; i++){
		xAxesScale.push(genMonthXScale(i));
	};
	
	for (var i = 1; i< 11; i++){
		xAxesScale.push(genYearXScale(i));
	};
	
	me[0].max = xAxesScale.length-1;
	
	var getValueByRange = function(intValue){
		var date, friquent;
		var xScaleVal = xAxesScale[intValue];
		date = xScaleVal.val + " " + xScaleVal.dim;
		return {date : date, friquent: friquent};
    };
	
	me.on('change', function(){
		me.updateValueLabel();
	});
	
	me.on('mouseup', function(){
		me.updateValueLabel();
	});

	me.updateValueLabel = function(){
		if (!NCI.chartData) {
			NCI.chartData = google.visualization.arrayToDataTable([
				['Time', 'NCI'],
				['sec', 0]
			]);
		};
		
		if (NCI.chartData.J.length > 0)
		    NCI.chartData.removeRows(0, NCI.chartData.J.length);	
			
	    var xScaleVal = xAxesScale[me[0].value];
	    var valIndex = xScaleVal.indexDim;
		var valDim = xScaleVal.indexDim;
		for (var i=0; i< xScaleVal.pointsNum; i++){
			NCI.chartData.insertRows(0, [[ xScaleVal.indexMaxVal*i/xScaleVal.pointsNum + " " + valDim, Math.floor((Math.random()*100)+1) ]]);
		};
		
		NCI.drawChart();
		NCI.periodLabel.html("<small> data for last </small> " + getValueByRange(parseInt(me[0].value)).date)
	};
	return me;
}());	

$('.slider .icon-plus-sign').on('click', function(){
	var curVal = NCI.slider[0].value;
	if (curVal < parseInt(NCI.slider[0].max)){
		NCI.slider[0].value = 1 + parseInt(NCI.slider[0].value);
		NCI.slider.updateValueLabel();
	};
});

$('.slider .icon-minus-sign').on('click', function(){
	var curVal = NCI.slider[0].value;
	if (curVal > 0){
	   NCI.slider[0].value = NCI.slider[0].value - 1;
	   NCI.slider.updateValueLabel();
    };
});

$('.round-info').on('touchend', function(){
	$('.round-info').trigger('click');
})

