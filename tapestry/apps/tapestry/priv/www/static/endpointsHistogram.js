(function() {

    var color = d3.scale.category20();

    function updateTooltip(container, item) {
        var tooltip = this.getTooltip(container);

        if (item) {
            tooltip.style("display", "block");

            tooltip.html([
                "<div>Endpoint: ",
                item.ip,
                "</div>",
                "<div>Activity: #",
                item.activity ? item.activity.index : "Activity not loaded",
                "</div>",
                "<div>Total Connections: ",
                item.totalConnections,
                "</div>",
                "<div>External Connections: ",
                item.externalConnections,
                "</div>",
                "<div>Internal Connections: ",
                item.internalConnections,
                "</div>"
            ].join(""));
        } else {
            tooltip.style("display", "none");
        }
    }

    function updateChart(container, data) {
        var self = this,
            chart = this.getChart(container),
            length = data.length,
            width = this.getSize().width,
            height = this.getSize().height,
            barWidth = width / length,
            maxConnections = Math.max.apply(null, data.map(function(item) {return item.totalConnections})),
        //            minConnection = Math.min.apply(null, data.map(function(item) {return item.totalConnections})),
            bar = chart.selectAll("g")
                .data(data, function(item) {return item.ip}),
            barEnter = bar.enter()
                .append("g")
                .classed("endpoint", true)
                .classed("external", function(d) {
                    return d.external;
                }),


            x = d3.scale.ordinal()
                .rangeRoundBands([0, width], .1)
                .domain(data.map(function(item) {return item.ip})),
            y = d3.scale.linear()
                .domain([0, maxConnections + 10])
                .range([height, 0]),
            xAxis = d3.svg.axis()
                .scale(x)
                .orient("bottom"),
            yAxis = d3.svg.axis()
                .scale(y)
                .tickFormat(d3.format("d"))
                .orient("left");


        bar.attr("transform", function(d, i) {
            return "translate(" + i * barWidth + ",0)";
        })
            .classed("mouse-over", false);

        barEnter.append("rect")
            .classed("internal-connections", true);

        barEnter.append("rect")
            .classed("external-connections", true);

        var internalRect = bar.select(".internal-connections"),
            externalRect = bar.select(".external-connections");


        internalRect
            .attr("transform", function(d) {
                return "translate(0, " + y(d.internalConnections) + ")";
            })
            .attr("height", function(d) {
                return height - y(d.internalConnections);
            })
            .attr("width", barWidth - 1)
            .attr("fill", color.range(2));

        externalRect
            .attr("transform", function(d) {
                return "translate(0, " + y(d.internalConnections + d.externalConnections) + ")";
            })
            .attr("height", function(d) {
                return height - y(d.externalConnections);
            })
            .attr("width", barWidth - 1)
            .attr("fill", color.range(2));

        var prevBar = null;

        bar.on("mouseover", function(d) {
            self.updateTooltip(container, d);
            d3.select(prevBar)
                .classed("mouse-over", false);
            d3.select(this)
                .classed("mouse-over", true);

            prevBar = this;
        });

        bar.on("click", function(d) {
            $(chart.node()).trigger("barClick", d);
        });

        bar.exit()
            .remove();

        bar.order();


        chart.append("g")
            .attr("class", "y axis")
            .call(yAxis);

    }


    function EndpointsHistogram(container, data) {
        NCI.HistogramUI.apply(this, arguments);
    }

    // subclass extends superclass
    EndpointsHistogram.prototype = Object.create(NCI.HistogramUI.prototype);
    EndpointsHistogram.prototype.constructor = EndpointsHistogram;

    EndpointsHistogram.prototype.updateChart = updateChart;
    EndpointsHistogram.prototype.updateTooltip = updateTooltip;

    NCI.EndpointsHistogram = EndpointsHistogram;
})();

