(function() {

    var defaultMargin = {top: 20, right: 70, bottom: 10, left: 120},
        width = 960 - defaultMargin.left - defaultMargin.right,
        height = 200 - defaultMargin.top - defaultMargin.bottom;

    function setWidth(wid, margin) {
        margin = margin || defaultMargin;
        width = wid - margin.left - margin.right;
    }

    function setHeight(hei, margin) {
        margin = margin || defaultMargin;
        height = hei - margin.top - margin.bottom;
    }

    function createTooltip(container) {
        container.append("div")
            .classed("histogram-tooltip", true)
            .style("display", "none");
    }

    function getTooltip(container) {
        return container.select(".histogram-tooltip");
    }

    function createHistogram(container, options) {

        var self = this,
            margin = options.margin || defaultMargin,
            histogram = container.append("div"),
            cc = histogram.append("div"),
            prev = cc.append("a")
                .text("<<")
                .classed("prev", true)
                .on("click", function() {
                    var options = chart.datum();
                    options.currentPage -= 1;
                    chart.datum(options);
                    self.updateHistogram(container, options.data);
                }),
            next = cc.append("a")
                .classed("next", true)
                .text(">>")
                .on("click", function() {
                    var options = chart.datum();
                    options.currentPage += 1;
                    chart.datum(options);
                    self.updateHistogram(container, options.data);
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

            setWidth(width, margin);
            setHeight(height, margin);
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
            if ($(container.node()).filter(":hidden").length == 0) {
                setWidthHeight(container);
                setSVGSize(svg);
                self.updateHistogram(container, chart.datum().data);
            }
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
        this.updateChart(container, currentData);
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

    function updateChart(container, data) {}

    function getChart(container) {
        return container.select(".chart");
    }

    // container is d3Selection
    function HistogramUI(container, data, options) {

        this.container = this.createHistogram(container, options || {});
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

    HistogramUI.ASC_DIRECTION = ASC_DIRECTION;
    HistogramUI.DESC_DIRECTION = DESC_DIRECTION;

    HistogramUI.prototype.setData = function(data) {
        this.data = data;
        this._update();
    };

    HistogramUI.prototype.updateChart = updateChart;
    HistogramUI.prototype.getChart = getChart;
    HistogramUI.prototype.updateHistogram = updateHistogram;
    HistogramUI.prototype.createHistogram = createHistogram;
    HistogramUI.prototype.getTooltip = getTooltip;

    HistogramUI.prototype.getSize = function() {
        return {
            width: width,
            height: height
        }
    };


    HistogramUI.prototype._update = function() {
        var currentData = this.data.slice();
        this.updateHistogram(this.container, currentData);
    };

    HistogramUI.prototype.sort = function(field, direction) {
        this.columns.forEach(function(column) {
            column.sort = null;
            if (column.property === field) {
                column.sort = direction;
            }
        });

        this._update();
    };

    HistogramUI.prototype.remove = function() {
        if (this.container) {
            this.container.on(".click");
            this.container.remove();
        }
        this.container = null;
        this.chart = null;
    };

    NCI.HistogramUI = HistogramUI;

})();
