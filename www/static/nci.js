if (typeof NCI === 'undefined')
   NCI = {};

NCI.nciLatestValue = $('#nciLatestValue');
NCI.nepLatestValue = $('#nepLatestValue');
NCI.qpsLatestValue = $('#qpsLatestValue');

NCI.ifMobile = function(){
	return /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(navigator.userAgent);
}

NCI.setNciLatestValue = function (newVal, time) {

		NCI.nciLatestValue.html('<name> NCI</name> <val> ' + newVal + ' </val> <br><i>updated &nbsp;' + time +'</i> ');
};

NCI.setNepLatestValue = function (newVal, time) {

		NCI.nepLatestValue.html('<name>Endpoints</name> <val>' + newVal + '</val> <br> <i>updated &nbsp;' + time +'</i>');
};

NCI.setQpsLatestValue = function (newVal, time) {
		NCI.qpsLatestValue.html('<name>Queries per Second </name><val>' + newVal + '</val> <br> <i> updated &nbsp;' + time +'</i>');
};

NCI.parceDataForLastUpdate = function(stringDate){
	var date = new Date(stringDate)
	var showDate = date.getMonth() + '.' + date.getDate() + '.' + date.getFullYear() % 100
		+ '  ' + date.getHours() + ':' + date.getMinutes() + ':' + date.getSeconds() ;
	return showDate;
};

NCI.parceNumberForView = function(labelValue){
    return Math.abs(Number(labelValue)) >= 1.0e+9

         ? Math.abs(Number(labelValue)) / 1.0e+9.toFixed(1) + "B"
         // Six Zeroes for Millions 
         : Math.abs(Number(labelValue)) >= 1.0e+6

         ? (labelValue/ 1.0e+6).toFixed(1)  + "M"
         // Three Zeroes for Thousands
         : Math.abs(Number(labelValue)) >= 1.0e+3

         ? (labelValue / 1.0e+3).toFixed(1) + "K"

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


NCI.sideMenuBtns = [NCI.infoBtn, NCI.settingsBtn];
NCI.selectedItem;

NCI.periodLabel = $('#periodLabel');

NCI.slider = (function(){
	var me =  $('#slider');
	
	var timeRanges = {
		ranges: [59, 
		59 + 23, 
		59 + 23 + 31, 
		59 + 23 + 31 + 11, 
		59 + 23 + 31 + 11 + 10],
		rangeNames : ['min', 'hours', 'days', 'mnths', 'years']
	};
	var getValueByRange = function(intValue){
		var date, friquent;
		$.each(timeRanges.ranges, function(index, range){
			if (intValue < range){
				var periodData = intValue + 1;
				if ( index !== 0 ){
					periodData -= timeRanges.ranges[index - 1];
				};
				friquent = index === 0;
			    date = periodData + " " + timeRanges.rangeNames[index];
				return false;
		    };
		});
		return {date : date, friquent: friquent};
    };
	
	me.on('change', function(){
		NCI.periodLabel.html("<small> data for last </small> " + getValueByRange(parseInt(me[0].value)).date)
	});
	
	me.on('mouseup', function(){
		NCI.periodLabel.html("<small> data for last </small> " + getValueByRange(parseInt(me[0].value)).date)
	});

	return me;
}());	

