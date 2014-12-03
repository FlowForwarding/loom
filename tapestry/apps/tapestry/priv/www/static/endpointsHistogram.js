(function() {

    var color = d3.scale.category20(),
        showHostname = false,
        margin = {top: 50, right: 70, bottom: 100, left: 100};

    $(NCI).on("showHostnames", function(e, show) {
        showHostname = show;
    });

        function updateTooltip(container, item) {
        var tooltip = this.getTooltip(container);

        if (item) {
            tooltip.style("display", "block");

            tooltip.html([
                "<div>Endpoint: ",
                item.ip,
                "</div>",
                showHostname ? ["<div>Host: ",
                                item.host,
                                "</div>"].join("") : "",
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
                .tickSize(width)
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

        chart.selectAll(".axis")
            .remove();

        chart.append("g")
            .attr("class", "x axis")
            .append("line")
            .attr({
                class: "axis-line",
                x1: 0,
                x2: width + 10,
                y1: height,
                y2: height
            });

        chart.append("g")
            .attr("class", "y axis")
            .attr('transform', 'translate(' + (width) + ')')
            .call(yAxis)
            .append("line")
            .attr({
                class: "axis-line",
                x1: -width,
                x2: -width,
                y1: -10,
                y2: height
            });
        ;

        var yAxisText = "Number of connected endpoints",
            xAxisText = "Endpoints sorted by number of connected endpoints";

        chart.selectAll(".axis-caption")
            .remove();

        chart.append('text')
            .classed("axis-caption", true)
            .attr('style', 'font-weight:bold')
            .html(xAxisText)
            .attr('transform', "translate(" + (width/2 - margin.left - 50) +", " + (height + 20) + ")"); // 30 is height of xaxis

        chart.append('text')
            .classed("axis-caption", true)
            .attr('style', 'font-weight:bold')
            .html(yAxisText)
            .attr('transform', "rotate(-90) translate(" + (-height + margin.top) + ", " + (-50) + ") ");

        chart.call(updateLegends, height);
    }


    function updateLegends(chart, height) {

        function createContainer(legends, text, cls) {
            var container = legends.append("g")
                .attr("class", cls);

            container.append("text")
                .text(text);

            return container;
        }

        function createLegendItem(container, cls, caption) {
            var legendContainer = container.append("g");

            legendContainer.append("rect")
                .attr({
                    x: 0,
                    y: 0,
                    width: "20",
                    height: "20",
                    class: cls,
                    transform: "translate(0, -15)"
                });

            legendContainer.append("text")
                .attr("transform", "translate(25, 0)")
                .text("- " + caption);

            return legendContainer;
        }

        chart.selectAll(".endpoints-legend")
            .remove();

        var legendsContainer = chart.append("g")
            .attr("class", "endpoints-legend")
            .attr("transform", "translate(0," + (height + 40) + ")");

        var internalContainer = createContainer(legendsContainer, "Internal endpoints:");

        var externalContainer = createContainer(legendsContainer, "External endpoints:", "external")
            .attr("transform", "translate(300, 0)");

        createLegendItem(externalContainer, "internal-connections", "Number of Internal connections")
            .attr("transform", "translate(0, 25)");
        createLegendItem(externalContainer, "external-connections", "Number of External connections")
            .attr("transform", "translate(0, 50)");

        createLegendItem(internalContainer, "internal-connections", "Number of Internal connections")
            .attr("transform", "translate(0, 25)");
        createLegendItem(internalContainer, "external-connections", "Number of External connections")
            .attr("transform", "translate(0, 50)");
    }

    function EndpointsHistogram(container, data, options) {
        options = options || {};
        options.margin = margin;

        NCI.HistogramUI.call(this, container, data, options);
    }

    // subclass extends superclass
    EndpointsHistogram.prototype = Object.create(NCI.HistogramUI.prototype);
    EndpointsHistogram.prototype.constructor = EndpointsHistogram;

    EndpointsHistogram.prototype.updateChart = updateChart;
    EndpointsHistogram.prototype.updateTooltip = updateTooltip;

    NCI.EndpointsHistogram = EndpointsHistogram;
})();

