NCI = {};

NCI.settingsPanel = (function(){
	var me =  $('#nciSettingsPanel');
	var saveBtn = me.find('#saveSettings');
	var collector = me.find('#collectorInput');
	var username = me.find('#usernameInput');
	var password = me.find('#passwordInput');
	var auth = me.find('#authOption');
	var makeDefault = me.find('#makeDefault');
	var alertBox = me.find('alert-box');
	
	auth.on('change', function(){
		username.prop('disabled', auth[0].checked);
		password.prop('disabled', auth[0].checked);
	});
	
	saveBtn.on('click', function(){
        $.ajax({
            type: 'POST',
            url: 'http://tut.by/auth',
            dataType: 'json',
			data: {},
            success: function(data){
				//update ui
            },
            error: function(xhr, type){
                alertBox.html('Error happened<a href="#" class="close">&times;</a>');
            }
        });
		
		console.log(collector.val());
	});
	
	return me;
}());

NCI.infoBtn  = (function(){
	var me =  $('#nciInfoBtn');
	me.panel = $('#nciInfoPanel');
	return me;
}());		

NCI.settingsBtn = (function(){
	var me =  $('#nciSettingsBtn');
	me.panel = NCI.settingsPanel;
	return me;
}());

NCI.playBtn = (function(){
	var me =  $('#nciPlayBtn');
	return me;
}());

NCI.sideMenuBtns = [NCI.infoBtn, NCI.settingsBtn];
NCI.selectedItem;

$.each(NCI.sideMenuBtns, function(index, btn){
	// if (btn.hasClass('active'))
	// 	NCI.selectedItem = btn;
	
	btn.on('click', function(){
		if (NCI.selectedItem){
			if (NCI.selectedItem !== btn){
				NCI.selectedItem.removeClass('active');
				if (NCI.selectedItem.panel){
					NCI.selectedItem.panel.removeClass('show-menupanel');
					NCI.selectedItem.panel.addClass('hide-menupanel');
				};
		    };
		};
		NCI.selectedItem = btn;
		if (btn.hasClass('active')){
			btn.removeClass('active');
			if (btn.panel){
				btn.panel.addClass('hide-menupanel');
				btn.panel.removeClass('show-menupanel');
			}
		} else {
			if (btn.panel){
				btn.addClass('active');
				btn.panel.addClass('show-menupanel');
				btn.panel.removeClass('hide-menupanel');
			};
		};
	});
});	

NCI.periodLabel = $('#periodLabel');
NCI.frequencyDropdown = $('#frequencyDropdown');