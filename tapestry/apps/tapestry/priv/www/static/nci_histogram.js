NCI.NCIHistogram = (function(){
    // d3.select("#nciHistogram");

    var color = d3.scale.category20(),
        margin = {top: 50, right: 70, bottom: 40, left: 100};

    function ticksValuesForLogScale(ticksCount) {
        return d3.range(Math.round(Math.log(ticksCount) / Math.LN10)).map(Math.pow.bind(null, 10));
    }

    function updateTooltip(container, item) {
        var tooltip = this.getTooltip(container);

        if (item) {
            tooltip.style("display", "block");

            tooltip.html([
                "<div>Activity: #",
                item.index,
                "</div>",
                //                "<div>Activity: #",
                //                item.activity ? item.activity.index : "Activity not loaded",
                //                "</div>",
                "<div>Size: ",
                item.size,
                "</div>",
//                "<div>Connections: ",
//                item.connections,
//                "</div>",
                //                "<div>Internal Connections: ",
                //                item.internalConnections,
                //                "</div>"
            ].join(""));
        } else {
            tooltip.style("display", "none");
        }
    }

    function updateChart(container, data){

        var chart = this.getChart(container),
            options = chart.datum(),
            currentPage = options.currentPage,
            itemsPerPage = options.itemsPerPage,
            endpointsMax = d3.max(data, function(activity) { return activity.size; }),
            size = this.getSize(),
            height = size.height,
            width = size.width,
            length = data.length,
            barMaxWidth = 10,
            barActualWidth = width / length,
            barWidth = Math.min(barActualWidth, barMaxWidth),
            barPadding = barActualWidth - barWidth - 1,
            endpointsMin,
            endpointsScale,
            endpointsAxisTicksValues;

        if (endpointsMax > 100) {
            endpointsMin = 0.5;
            endpointsScale = d3.scale.log();
            endpointsAxisTicksValues = ticksValuesForLogScale(endpointsMax);
        } else {
            endpointsMin = 0;
            endpointsScale = d3.scale.linear();
            endpointsAxisTicksValues = null;
        }

        endpointsScale.domain([endpointsMax, endpointsMin])
            .range([0, height]);

        var activitiesScale = d3.scale.linear(),
            activitiesMin = 0,
            activitiesAxisTickValues;

        if (length < 800){
            activitiesScale = d3.scale.linear();
            activitiesMin = (currentPage) * itemsPerPage;
            activitiesAxisTickValues = null;
        } else{
            activitiesScale = d3.scale.log();
            activitiesMin = 0.5;
            activitiesAxisTickValues = ticksValuesForLogScale(length);
        };
        activitiesScale.domain([(currentPage + 1) * itemsPerPage, activitiesMin])
                        .range([width, 0]);

        var activitiesAxis = d3.svg.axis()
            .scale(activitiesScale)
            .orient('bottom')
            .tickSize(0)
            .ticks(Math.min(length + 2, 10),  d3.format("d"))
            .tickValues(activitiesAxisTickValues)
            .tickPadding(8);

        var endpointsAxis = d3.svg.axis()
            .scale(endpointsScale)
            .orient('left')
            .tickSize(width)
            .ticks(10,  d3.format("d"))
            .tickValues(endpointsAxisTicksValues)
            .tickPadding(8);

        chart.selectAll(".axis")
            .remove();

        //draw axis
        chart.append('g')
            .attr('class', 'x axis')
            .attr('transform', 'translate(' + (-barPadding) + ',' + (height) + ')')
            .call(activitiesAxis);

        chart.insert('g')
            .attr('class', 'y axis')
            .attr('transform', 'translate(' + (width - barPadding) + ')')
            .call(endpointsAxis)
            .append("line")
            .attr({
                class: "axis-line",
                x1: -width,
                x2: -width,
                y1: -10,
                y2: height
            });


        //draw bars
        var bar = chart.selectAll('.activity')
                .data(data, function(d) {return d.index}),
            barEnter = bar.enter()
                .append("g")
                .classed("activity", true);

        bar.attr("transform", function(d, i) {
            return "translate(" + activitiesScale(i) + ",0)";
        });

        barEnter
            .append('rect')
            .classed('activity-bar', true)
            .attr("fill", color());

//            .on("click", function(d) {
//				if (NCI.nciHistogram.selectedBar)
//				    NCI.nciHistogram.selectedBar.setAttribute("fill", "black");
//				NCI.nciHistogram.selectedBar = this;
//				this.setAttribute("fill", "rgb(31, 119, 180)");
//				me.showDetails(d);
//			 });

        bar.selectAll(".activity-bar")
            .attr("transform", function(d) {
                return "translate(0, " + endpointsScale(d.size) + ")";
            })
            .attr("height", function(d) {
                return height - endpointsScale(d.size);
            })
            .attr("width", barWidth)
//            .attr("fill", color.range(2));

        function createSmt(selection, smth) {
            console.log(arguments);
        }

        bar.exit()
            .remove();

        var prevBar = null,
            self = this;

        bar.on("mouseover", function(d) {
            self.updateTooltip(container, d);
            d3.select(prevBar)
                .classed("mouse-over", false);
            d3.select(this)
                .classed("mouse-over", true);

            prevBar = this;
        });

        bar.on("click", function(d) {
            showDetails(d);
//            $(chart.node()).trigger("barClick", d);
        });

        chart.selectAll("circle")
            .remove();

        chart.append("circle")
            .attr("cy", endpointsScale(NCI.timestampNCI))
            .attr("cx", activitiesScale(NCI.timestampNCI) - barPadding - barWidth/2)
            .style("fill", "red")
            .attr("r", 6);
        chart.append("circle")
            .attr("cy", endpointsScale(endpointsMin))
            .attr("cx", activitiesScale(NCI.timestampNCI) - barPadding - barWidth/2)
            .style("fill", "red")
            .attr("r", 4);
        chart.append("circle")
            .attr("cy", endpointsScale(NCI.timestampNCI))
            .attr("cx", activitiesScale(activitiesMin) - barPadding)
            .style("fill", "red")
            .attr("r", 4);

        //draw axis labels

        chart.selectAll(".axis-caption")
            .remove();

        chart.append('text')
            .classed("axis-caption", true)
            .attr('style', 'font-weight:bold')
            .html('Activities Sorted by Size')
            .attr('transform', "translate(" + (width/2 - margin.left) +", " + (height + 30) + ")"); // 30 is height of xaxis

        chart.append('text')
            .classed("axis-caption", true)
            .attr('style', 'font-weight:bold')
            .html('Number of Endpoints per Activity X[j]')
            .attr('transform', "rotate(-90) translate(" + (-height + margin.top) + ", " + (-50) + ") ");

    };

    function showDetails(activity) {

        $('.histogramDetailsClose').on('click', function(){
            clean();
            $('.histogram-details-graph').hide();
            $('.histogramDetailsClose').off("click");
        });

        $('.histogram-details-graph').show();

        //		NCI.MouseClickActivitySound.currentTime = 0;
        //		NCI.MouseClickActivitySound.play();

        var color = d3.scale.category10(),
            community = NCI.Communities.filter(function(community) {return community.Label==activity.mainEndpoint.ip})[0],
            socialGraph = new NCI.socialGraph(".histogram-details-graph", {
                communities: [community],
                numOfPoints: community.Endpoints.length,
                width: $(window).width(),
                height: $(window).height(),
                isClustered: true,
                legendData : [
                    [color(0), " - endpoint in activity"],
                    ["red", " - endpoint in a different activity"],
                    [NCI.notNetworkColor, " - external endpoint"]
                ],
                radius: function(){
                    var verticlesLimitForRadius = 250
                    var radius = 4
                    if (community.Endpoints.length < verticlesLimitForRadius) {
                        radius = 3 + 2 - 2*community.Endpoints.length/verticlesLimitForRadius;
                    }
                    return radius;
                },
                linkDistance: function(){
                    return (5 + 160 * (NCI.max_vertices - community.Endpoints.length)/NCI.max_vertices);
                }
            });

        socialGraph.show(true);

        function clean() {
            if (socialGraph) {
                socialGraph.clean();
            }
        };

    }

    function NCIHistogram(container, data, options) {
        options = options || {};
        options.margin = margin;
        NCI.HistogramUI.call(this, container, data, options);
    }


    NCIHistogram.prototype = Object.create(NCI.HistogramUI.prototype);
    NCIHistogram.prototype.constructor = NCIHistogram;

    NCIHistogram.prototype.updateChart = updateChart;
    NCIHistogram.prototype.updateTooltip = updateTooltip;

    return NCIHistogram;
}());
