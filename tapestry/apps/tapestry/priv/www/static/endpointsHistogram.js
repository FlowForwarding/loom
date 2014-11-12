(function() {

    var margin = {top: 20, right: 70, bottom: 10, left: 120},
        width = 960 - margin.left - margin.right,
        height = 200 - margin.top - margin.bottom,
        color = d3.scale.category20();

    function setWidth(wid) {
        width = wid - margin.left - margin.right;
    }

    function setHeight(hei) {
        height = hei - margin.top - margin.bottom;
    }

    function createTooltip(container) {
        container.append("div")
            .classed("histogram-tooltip", true);
    }

    function updateTooltip(container, item) {
        var tooltip = container.select(".histogram-tooltip");

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

    function createHistogram(container, options) {

        var histogram = container.append("div"),
            cc = histogram.append("div"),
            prev = cc.append("a")
                .text("<<")
                .classed("prev", true)
                .on("click", function() {
                    var options = chart.datum();
                    options.currentPage -= 1;
                    chart.datum(options);
                    updateHistogram(container, options.data);
                }),
            next = cc.append("a")
                .classed("next", true)
                .text(">>")
                .on("click", function() {
                    var options = chart.datum();
                    options.currentPage += 1;
                    chart.datum(options);
                    updateHistogram(container, options.data);
                }),
            svg = cc.insert("svg", ".next"),
            chart = svg.append("g")
                .classed("chart", true)
                .attr("transform", "translate(" + margin.left + "," + margin.top + ")"),
            pager = histogram.append("div")
                .classed("page-indicator", true)
                .text("this is pager");


            function setSVGSize(svg) {
            svg.attr("width", width + margin.left + margin.right - 10
                - parseInt(prev.style("width"), 0)
                - parseInt(next.style("width"), 0))
                .attr("height", height + margin.top + margin.bottom);
        }

        function setWidthHeight(container) {
            var $container = $(container.node()),
                width = $container.width(),
                height = $container.height();

            setWidth(width);
            setHeight(height);
        }

        setWidthHeight(container);
        setSVGSize(svg);

        createTooltip(histogram);

        options = options || {}

        var currentPage = options.currentPage || 0,
            itemsPerPage = options.itemsPerPage || 50;

        chart.datum({
            currentPage: currentPage,
            itemsPerPage: itemsPerPage,
            data: []
        });

        $(window).resize(function() {
            setWidthHeight(container);
            setSVGSize(svg);
            updateHistogram(container, chart.datum().data);
        });

        return histogram;
    }

    function updateHistogram(container, data) {
        var chart = getChart(container),
            config = chart.datum(),
            currentPage,
            itemsPerPage = config.itemsPerPage,
            currentData;


        if (config.data!==data) {
            config.currentPage = 0;
        }

        currentPage = config.currentPage;
        currentData = data.slice(currentPage * itemsPerPage, (currentPage + 1) * itemsPerPage);

        config.data = data;
        chart.datum(config);

        updateControls(container);
        updateChart(container, currentData);
    }

    function updateControls(container) {
        var chart = getChart(container),
            options = chart.datum(),
            next = container.select(".next"),
            pageIndicator = container.select(".page-indicator"),
            prev = container.select(".prev"),
            maxItem = (options.currentPage + 1) * options.itemsPerPage,
            dataLength = options.data.length;

        prev.style("visibility", "visible");
        next.style("visibility", "visible");

        if (options.currentPage == 0) {
            prev.style("visibility", "hidden");
        }

        if (maxItem >= dataLength) {
            next.style("visibility", "hidden");
        }

        var text = [
            options.currentPage * options.itemsPerPage + 1,
            " - ",
            maxItem >= dataLength ? dataLength : maxItem,
            ". Out of ",
            options.data.length
        ].join("");

        pageIndicator.text(text);

    }

    function updateChart(container, data) {
        var chart = getChart(container),
            length = data.length,
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

//        barEnter.append("text")
//            .attr("fill", "black")
//            .attr("transform", "translate(" + (barWidth/2 + 4) + ", " + (height - 8) + ") rotate(-90)")
//            .text(function(d) {
//                return d.ip;
//            });

        var prevBar = null;

        bar.on("mouseover", function(d) {
            updateTooltip(container, d);
            d3.select(this)
                .classed("mouse-over", true);
            d3.select(prevBar)
                .classed("mouse-over", false);

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

//        chart.append("g")
//            .attr("class", "x axis")
//            .attr("transform", "translate(0," + height + ")")
//            .call(xAxis);

//        bar.append("text")
//            .attr("x", barWidth / 2)
//            .attr("y", function(d) { return y(d.value) + 3; })
//            .attr("dy", ".75em")
//            .text(function(d) { return d.value; });
    }

    function getChart(container) {
        return container.select(".chart");
    }

    // container is d3Selection
    function EndpointsHistogram(container, data) {

        this.container = createHistogram(container);
        this.chart = getChart(this.container);

        var $me = $(this);

        $(this.chart.node()).on("barClick", function(event, data) {
            $me.trigger("click", data);
            console.log(data);
        });

        this.setData(data || null);
    }

    var ASC_DIRECTION = 1,
        DESC_DIRECTION = -1;

    EndpointsHistogram.ASC_DIRECTION = ASC_DIRECTION;
    EndpointsHistogram.DESC_DIRECTION = DESC_DIRECTION;

    EndpointsHistogram.prototype.setData = function(data) {
        this.data = data;
        this._update();
    };


    EndpointsHistogram.prototype.filter = function(field, filter) {
        updateFilter(this.columns, field, filter);
        this._update();
    };

    EndpointsHistogram.prototype._update = function() {
        var currentData = this.data.slice();
//            .sort(function(endpointA, endpointB) {
//                return endpointB.totalConnections - endpointA.totalConnections;
//            });
            //,
//            columns = this.columns;

//        columns.forEach(function(column) {
//            if (column.filter) {
//                currentData = filterData(currentData, column.property, column.filter);
//            }
//        });
//
//        columns.forEach(function(column) {
//            var sortFn = column.sortFn || defaultSortFn;
//            if (column.sort) {
//                currentData = sortData(currentData, column.property, column.sort, sortFn);
//            }
//        });

        updateHistogram(this.container, currentData);
    };

    EndpointsHistogram.prototype.sort = function(field, direction) {
        this.columns.forEach(function(column) {
            column.sort = null;
            if (column.property === field) {
                column.sort = direction;
            }
        });

        this._update();
    };

    EndpointsHistogram.prototype.remove = function() {
        if (this.container) {
            this.container.on(".click");
            this.container.remove();
        }
        this.container = null;
        this.chart = null;
    };

    NCI.EndpointsHistogram = EndpointsHistogram;
})();
