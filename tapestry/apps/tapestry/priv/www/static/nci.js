if (typeof NCI === 'undefined')
   NCI = {};
 
NCI.nciLatestValue = $('#nciLatestValue');
NCI.nepLatestValue = $('#nepLatestValue');
NCI.qpsLatestValue = $('#qpsLatestValue');
NCI.collectorsLatestValue = $('#collectorsLatestValue');
NCI.lastUpdateTime = $('#lastUpdateTime');

NCI.ifMobile = function(){
	return /Android|webOS|iPhone|iPad|iPod|BlackBerry/i.test(navigator.userAgent);
};

NCI.currentNCI = 0;
NCI.collectors = [];
NCI.collectorsUpdateDate = "";
NCI.nciUpdateDate = "";

NCI.setNciLatestValue = function (val, time, activities) {
	NCI.nciUpdateDate = time;
	var colorClass = val > NCI.currentNCI ? 'green' : val == NCI.currentNCI ? 'black' : 'red';
	NCI.currentNCI = val;
	var newVal = NCI.parceNumberForView(val);
	NCI.nciLatestValue.html('<val class="' + colorClass + '"> ' + newVal + ' </val><br><i>updated &nbsp;' + time + '</i> ');
	NCI.lastUpdateTime.html('updated &nbsp;' + time);
};

NCI.setNepLatestValue = function (newVal, time) {
	NCI.nepLatestValue.html('<val>' + newVal + '</val><br><i>updated &nbsp;' + time + '</i>');
};

NCI.setQpsLatestValue = function (newVal, time) {
	NCI.qpsLatestValue.html('<val>' + newVal + '</val> <br><i>updated &nbsp;' + time + '</i>');
};

NCI.setCollectorsLatestValue = function (collectors, time) {
	var newVal = NCI.parceNumberForView(collectors.length);
	NCI.collectors = collectors;
	NCI.collectorsUpdateDate =  time;
	NCI.collectorsLatestValue.html('<val>' + newVal + 
	    '</val> <br><i>updated &nbsp;' + NCI.parceDateForLastUpdate(time) + '</i>');
};

NCI.convertDateForServer = function(date){
	//we need to get such format in UTC 2013-10-27T13:11:39Z for server
	var returnDate = new Date(date).toISOString();
	returnDate = returnDate.substring(0, returnDate.length - 5) + "Z";
	return returnDate;
};

NCI.parceDateForLastUpdate = function(stringDate){
	var date = new Date(new Date(stringDate) - NCI.time_adjustment);
	return NCI.dateString_(date);
};

NCI.parceNumberForView = function(labelValue, fixVal){
    return Math.abs(Number(labelValue)) >= 1.0e+9

         ? Math.abs(Number(labelValue)) / 1.0e+9.toFixed(1) + " B"
         : Math.abs(Number(labelValue)) >= 1.0e+6

         ? (labelValue/ 1.0e+6).toFixed(1)  + " M"
         : Math.abs(Number(labelValue)) >= 1.0e+3

         ? (labelValue / 1.0e+3).toFixed(1) + " K"
         : Math.abs(Number(labelValue)).toFixed(fixVal);
};

$('.round-info').on('touchend', function(){
	$('.round-info').trigger('click');
})

NCI.zoomLinks = (function(){
	var me = $('.zoom-panel a');
	
	me.setTimePeriod = function(period){
		var foundDisabled = false;
		$.each(me, function(index, link){
			if (period - link.dataset.time < 0){
				if (foundDisabled)
					$(link).addClass('disabled');
				foundDisabled = true;	
			}
		});
	};
	
	me.on('click', function(){
		if ($(this).hasClass('disabled'))
			return;
		NCI.zoomLinks.removeClass('selected');
		
		if ((NCI.curChartPeriod <= NCI.chartPeriods.twoyears && this.dataset.time <= NCI.chartPeriods.twoyears) ||
			(NCI.curChartPeriod  > NCI.chartPeriods.tenyears && this.dataset.time > NCI.chartPeriods.tenyears)){
			var ranges = NCI.DetectRangesForPeiod( this.dataset.time, NCI.chartData);		
			NCI.chart.updateOptions({
				dateWindow: [ ranges[0],  ranges[1]]
			});
		} else {
			NCI.chartData = [];
			NCI.chart.updateOptions({file: NCI.chartData});
			NCI.curChartPeriod =  this.dataset.time <= NCI.chartPeriods.twoyears ? NCI.chartPeriods.twoyears : NCI.chartPeriods.tenyears;
			NCI.detailedChartPeriod = this.dataset.time;
			NCI.Connection.moreData(new Date() - NCI.curChartPeriod, new Date(), NCI.numOfPoints);
		};

		$(this).addClass('selected');
	});
	
	return me;
}());

NCI.collectorsTable = (function(){
	var me = $('#collectorsInfo');
	var table = me.find("#collectorsTableBody");
	var pagination = me.find("#collectorsPagination");
	var collectorsgeneral = me.find(".collectorsGeneral");
	var numOnPage = 10;
	var collectors = [];
	var pages = [];
	var currentPage;
	
	me.showDataForPage = function(page){
		var lastIndex = numOnPage*page  > collectors.length ? collectors.length : numOnPage*page;
		var content = "";
	    for (var i = numOnPage*(page-1); i< lastIndex; i++){
	   	    var collectorInfo = collectors[i];
			content += "<tr><td>" +  collectorInfo.name + "</td><td>" +  
			collectorInfo.ip + "</td><td>" +  
			collectorInfo.datapath_id + "</td><td>" +  
			NCI.parceNumberForView(collectorInfo.qps)  + "</td><td>" +  
			collectorInfo.collector_type + "</td></tr>";
		};
		table.html(content);
	};
	
	me.on('click', '.pager', function(){
		me.showDataForPage(this.dataset.page)
		currentPage.removeClass("current");
		currentPage = $(this);
		currentPage.addClass("current");
	});
	
	me.fillData = function(collectorsArray){
		collectorsgeneral.html(collectorsArray.length + " collectors at &nbsp;&nbsp;" + NCI.collectorsUpdateDate);
		collectors = collectorsArray;
		
	  // this is dots
	  // <li class="unavailable"><a href="">&hellip;</a></li>
		
		var pageCount = collectors.length/numOnPage;
		if (pageCount > 1) {
			if (pageCount > Math.round(pageCount)) {
				pageCount = Math.round(pageCount) + 1;
			} else {
				pageCount = Math.round(pageCount);
			}
			var paginationContent = "<li class='arrow unavailable'><a>&laquo;</a></li>"
			for (var i = 0; i < pageCount; i++){
				paginationContent += '<li class="pager" data-page="' + (i + 1) + '"><a>' + (i + 1) + '</a></li>';
			}
			paginationContent += "<li class='arrow'><a>&raquo;</a></li>"
			pagination.html(paginationContent);
		} else {
			pagination.html("");
		}
		pages = pagination.find("li");
		me.showDataForPage(1);
		if (pages.length > 0){
			currentPage = $(pages[1]);
			currentPage.addClass("current");	
		}

	}
	
	return me;
}());

NCI.setupCommunities = function(data){
	NCI.Communities = data.Communities;
	NCI.Communities.sort(function(a, b){
		return a.Endpoints.length < b.Endpoints.length;
	});
	NCI.timestampNCI = data.NCI;
	NCI.timestamp = data.Time;
	$("#histogramGeneral").html("<b>NETWORK COMPLEXITY INDEX at &nbsp;&nbsp;</b> <i>" + NCI.parceDateForLastUpdate(NCI.timestamp) + "</i>" +
	    "&nbsp;&nbsp;&nbsp;<span class='button alert'>NCI " + NCI.timestampNCI + "</span>" );
};

NCI.nciHistogram = (function(){
	var me = $('#nciHistogram');
	
	var barHeight = 5;
	var chart = d3.select("#nciHistogram");
	var margin = {top: 40, right: 40, bottom: 40, left:40},
	    width = 600,
	    height = 300;
	
	me.show = function(){
		chart.text("");
		var x = d3.scale.linear()
		    .domain([0, d3.max(NCI.Communities, function(d) { return d.Endpoints.length; })])
		    .range([width - margin.left - margin.right, 0]);

		var y = d3.scale.linear()
		    .domain([0, NCI.Communities.length])
		    .range([height - margin.top - margin.bottom, 0]);

		var xAxis = d3.svg.axis()
		    .scale(x)
		    .orient('bottom')
		    .tickSize(0)
			.tickFormat(d3.format("d"))
		    .tickPadding(8);

		var yAxis = d3.svg.axis()
		    .scale(y)
		    .orient('right')
			.tickSize(0)
			.tickFormat(d3.format("d"))
		    .tickPadding(10);

		var barChartSvg = chart.append('svg')
		    .attr('width', width)
		    .attr('height', height)
		    .append('g')
		    .attr('transform', 'translate(' + margin.left + ', ' + margin.top + ')');

        //draw and animate bars
        var index = 1;
		barChartSvg.selectAll('g')
		    .data(NCI.Communities)
		    .enter().append('rect')
		    .attr('x', function(d) { return   0 })
		    .attr('y', function(d) { return  y(index++)}) //- selfwidth
		    .attr('width', function(d) { return 0})
		    .attr('height', barHeight)
			.on("click", me.showDetails);
		barChartSvg.selectAll('rect').data(NCI.Communities).transition()
		    .duration(1000)
			.attr("width", function(d) { return width - margin.left - margin.right -x(d.Endpoints.length)})
			.attr('x', function(d) { return   x(d.Endpoints.length) });		
		//draw NCI point	
		barChartSvg.append("circle").attr("cy", y(NCI.timestampNCI))
		    .attr("cx", x(NCI.timestampNCI) ).style("fill", "red").attr("r", 6);	
	    barChartSvg.append("circle").attr("cy", y(0))
		    .attr("cx", x(NCI.timestampNCI) ).style("fill", "red").attr("r", 4);
		barChartSvg.append("circle").attr("cy", y(NCI.timestampNCI))
		    .attr("cx", x(0) ).style("fill", "red").attr("r", 4);
		//draw axis 	
		barChartSvg.append('g')
		    .attr('class', 'x axis')
			.attr('transform', 'translate(0, ' + (height - margin.top - margin.bottom) + ')')
			.call(xAxis);
		barChartSvg.append('g')
			.attr('class', 'y axis')
			.attr('transform', 'translate(' + (width - margin.right - margin.left) + ')')
			.call(yAxis);	
		//draw axis labels																					
		barChartSvg.append('text').text('Number of Endpoints per Activity').attr('x', width/2 - 150).attr('y', height - 45);
		barChartSvg.append('text').text('Activities Sorted by Size').attr('x', -height/2 - 20).attr('y', width - 50)
		.attr('transform', 'rotate(-90)');

	};
	
	me.showDetails = function(d){
		chart.select("#bar_endpoints").remove();
		var activityDetails = chart.append('svg')
			.attr("id","bar_endpoints")
			.attr('width', height)
		    .attr('height', height);
			
	    var graph = NCI.prepareDataForForceGraph([d]);
		
		var force = d3.layout.force()
			.charge(-60)
			.linkDistance(30)
			.size([ height, height])
			.linkStrength(1).nodes(graph.nodes).links(graph.links).start();
			
		var link = activityDetails.selectAll(".link")
		    .data(graph.links)
			.enter().append("line")
			.attr("class", "link")
			.style("stroke-width", function(d) { return Math.sqrt(d.value); });  	
				  
		var node = activityDetails.selectAll(".node")
		    .data(graph.nodes)
		    .enter().append("circle")
		    .attr("r", 5)
		    .call(force.drag);
		  		  
		node.append("title").text(function(d) { return d.name; });

		force.on("tick", function() { 
			link.attr("x1", function(d) { return d.source.x; })
			    .attr("y1", function(d) { return d.source.y; })
				.attr("x2", function(d) { return d.target.x; })
				.attr("y2", function(d) { return d.target.y; });
				
			node.attr("cx", function(d) { return d.x; })
			    .attr("cy", function(d) { return d.y; });
		});	  
	};
	
	return me;
}());

NCI.socialGraph  = (function(){
	var me = $("#socialGraph");
	
	var force;
	var color = d3.scale.category10();
	me.clustered = false;
	var networkColor = "#000000";
	
	me.show = function(devided, clustered, filtered){
		if (me.clustered && !clustered){
			NCI.socialGraph.text("");
			me.draw(devided, clustered);
			me.clustered  = clustered;
			return;
		}
		me.clustered = clustered;
		if (me.text().length<20){	
			me.draw(devided, clustered, filtered);
		} else {
			me.node.style("fill", function(d) { 
  			    if ( filtered && (d.name.search("10.") == 0 ||  d.name.search("192.168") == 0)){
  				    return networkColor;
  			    }
  			    return devided ? color(d.group) : color(0);
			});
			me.node.attr("r", function(d) { return (filtered &&  (d.name.search("10.") == 0 ||  d.name.search("192.168") == 0)) ? 7 : 5})
			force.linkStrength(clustered ? 1 : 0);
	  	    force.start();
		}
	};
	
	me.draw = function(devided, clustered, filtered){
		d3.select("#activities_graph").remove();
	    me.graph = NCI.prepareDataForForceGraph(NCI.Communities);
		
		force = d3.layout.force()
			.charge(-60)
			.linkDistance(30)
			.size([ me.width(), $('#nciDetails').height() - 150])
			.linkStrength(clustered ? 1 : 0)
			.nodes(me.graph.nodes).links(me.graph.links).start();
			
	    me.activitiesGraphSvg = d3.select("#socialGraph").append("svg")
		    .attr("id","activities_graph")
			.attr("width", me.width())
			.attr("height", me.height());
	  
		var link = me.activitiesGraphSvg.selectAll(".link")
		    .data(me.graph.links)
			.enter().append("line")
			.attr("class", "link")
			.style("stroke-width", function(d) { return Math.sqrt(d.value); });  	
		  
		me.node = me.activitiesGraphSvg.selectAll(".node")
		    .data(me.graph.nodes)
			.enter().append("circle")
			.attr("class", "node")
			.attr("r", function(d) { return (filtered &&  (d.name.search("10.") == 0 ||  d.name.search("192.168") == 0)) ? 7 : 5})
			.style("fill", function(d) { 
				if ( filtered &&  (d.name.search("10.") == 0 ||  d.name.search("192.168") == 0)){
				  return networkColor;
			    }
				return devided ? color(d.group) : color(0);
			})
			.call(force.drag);
		
		me.node.append("title").text(function(d) { return d.name; });

	    force.on("tick", function() { 
	        link.attr("x1", function(d) { return d.source.x; })
	            .attr("y1", function(d) { return d.source.y; })
	            .attr("x2", function(d) { return d.target.x; })
	            .attr("y2", function(d) { return d.target.y; });

	        me.node.attr("cx", function(d) { return d.x; })
	            .attr("cy", function(d) { return d.y; });
		});
	}
	
	return me;
}());

NCI.prepareDataForForceGraph = function(communities){
    var graph = { "nodes":[], "links": []};
	
	var nodeIndex = function(ip){
		var val = 0;
		$.each(graph.nodes, function(index, node){
			if (node.name == ip)
			   val = index;
		});
		return val;
	};	
	
	$.each(communities, function(index, community){
		$.each(community.Endpoints, function(index2, endpoint){
			graph.nodes.push({
				"name": endpoint,
				"group": index
			});
		});
		$.each(community.Interactions, function(index2, interacton){
			graph.links.push({
				"source": nodeIndex(interacton[1]),
				"target": nodeIndex(interacton[0]),
				"value":1});
		});
	});
	
	return graph;
};

$(document).on('opened', '#nciDetails', function () {
	$(this).height($(window).height());
	$(this).css({'top': '0px'});
	NCI.Connection.NCIDetails(NCI.nciUpdateDateServer);
});

$(document).on('close', '#nciDetails', function () {
	NCI.socialGraph.text("");
	NCI.nciHistogram.text("");
	$('#nciDetailsTabs').find("a").first().click();
	
});

$(document).on('opened', '#collectorsInfo', function () {
	$(this).height($(window).height());
	$(this).css({'top': '0px'});
	NCI.Connection.CollectorsDetails(NCI.nciUpdateDateServer);
});

$('body').on('touchend', function(){
	$('.tooltip').hide();
});

$('#nciDetailsTabs').on('toggled', function (event, tab) {
	if (tab[0].id == "panelFlows"){
		NCI.socialGraph.show( false, false);
	} else if(tab[0].id == "panelActivities"){
	    NCI.socialGraph.show(true, false);
	} else if (tab[0].id == "panelActivitiesPretty"){
		NCI.socialGraph.show(true, true);
	} else if (tab[0].id == "panelInternalNetwork"){
		NCI.socialGraph.show(true, true, true);
	} else {
		NCI.nciHistogram.show();
		NCI.socialGraph.text("");
	}
});
