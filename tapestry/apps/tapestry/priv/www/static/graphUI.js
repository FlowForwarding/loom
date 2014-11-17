(function () {

    var width = 960,
        height = 200;


    function createGraph(container, options) {

        var graph = container.append("div"),
            svg = graph.append("svg"),

            chart = svg.append("g")
                .classed("graph", true);

        function setSVGSize() {
            svg.attr("width", width)
                .attr("height", height);
        }

        function setWidthHeight(container) {
            var $container = $(container.node());

            width = $container.width();
            height = $container.height();
        }

        setWidthHeight(container);
        setSVGSize();

//        createTooltip(graph);

//        options = options || {}

//        var currentPage = options.currentPage || 0,
//            itemsPerPage = options.itemsPerPage || 50;

        chart.datum({
//            currentPage: currentPage,
//            itemsPerPage: itemsPerPage,
            data: []
        });

        $(window).resize(function() {
            if ($(container.node()).filter(":hidden").length == 0) {
                setWidthHeight(container);
                setSVGSize();
//                updateGraph(container, chart.datum().data);
            }
        });

        return graph;


    }

    function selectGraph(container) {
        return container.select(".graph");
    }

    function updateGraph(container, data) {
        var graph = selectGraph(container);

        var color = d3.scale.category20();

        if (this.force) {
            this.force.stop();
        }

        this.force = d3.layout.force()
            .charge(-120)
            .linkDistance(30)
            .size([width, height]);

        var link = graph.selectAll(".link")
            .data(data.links, function(d) {return d.index});

        link.enter()
            .append("line")
            .attr("class", "link")
            .style("stroke-width", function(d) { return Math.sqrt(d.value); });

        link.exit()
            .remove();

        var node = graph.selectAll(".node")
            .data(data.nodes, function(d) {return d.name}),
            nodeEnter = node.enter()
                .append("circle");

        nodeEnter
            .attr("class", "node")
            .attr("r", 5)
            .style("fill", function(d) { return color(d.group); })
            .on("click", function(data) {
                $(graph.node()).trigger("nodeClick", data);
            });

        nodeEnter
            .append("title")
            .text(function(d) { return d.name; });

        node.exit()
            .remove();

        node.order();
        link.order();

        this.force
            .nodes(data.nodes)
            .links(data.links)
            .start()
            .stop();

        node.call(this.force.drag);

        this.force.on("tick", function() {
            link.attr("x1", function(d) { return d.source.x; })
                .attr("y1", function(d) { return d.source.y; })
                .attr("x2", function(d) { return d.target.x; })
                .attr("y2", function(d) { return d.target.y; });

            node.attr("transform", function(d) {
                return "translate(" + d.x + ", " + d.y + ")";
            });
//                .attr("transform", function(d) { return d.y; });

        });
    }

    function Graph(container, data, options) {
        this.container = createGraph(container, options);
        this.data = null;
        this.force = null;

        this.graph = selectGraph(this.container);

        var $me = $(this);

        $(this.graph.node()).on("nodeClick", function(event, data) {
            var ip = data.name;
            $me.trigger("click", NCI.model.getEndpointByIp(ip));
            console.log(data);
        });


        this.setData(data);
    }

    Graph.prototype.stop = function() {
        this.force.stop();
    };

    Graph.prototype.resume = function() {
        this.force.resume();
    };

    Graph.prototype.setData = function(data) {
        this.data = buildGraphData(data);
        this._update();
    };

    Graph.prototype.updateGraph = updateGraph;

    Graph.prototype._update = function() {
        this.updateGraph(this.container, this.data);
    };

    Graph.prototype.remove = function() {
        if (this.container) {
            this.container.on(".click");
            this.container.remove();
        }
        if (this.force) {
            this.force.stop();
            this.force = null;
        }
        this.graph = null;
        this.container = null;
    };

    function buildGraphData(data) {
        var nodes = [],
            links = [],
            indexMap = {};

        data.forEach(function (endpoint, index) {
            indexMap[endpoint.ip] = index;
            nodes.push({
                name: endpoint.ip,
                group: endpoint.activity.index
            });
        });

        data.forEach(function (endpoint) {
            var sourceIndex = indexMap[endpoint.ip];
            Object.keys(endpoint.connections).forEach(function (ip) {
                var targetIndex = indexMap[ip];
                if (targetIndex !== null && targetIndex !== undefined) {
                    links.push({
                        source: sourceIndex,
                        target: targetIndex,
                        value: 1,
                        index: [sourceIndex, targetIndex].join("|")
                    });
                }
            });

            indexMap[endpoint.ip] = null;
        });

        return {nodes: nodes, links: links};
    }


    NCI.Graph = Graph;
})();

function buildVivaGraph(data) {
    var nodes = [],
        links = [],
        indexMap = {};

    data.forEach(function (endpoint, index) {
        indexMap[endpoint.ip] = index;
        nodes.push({
            name: endpoint.ip,
            group: endpoint.activity.index
        });
    });

    var graph = Viva.Graph.graph();

    data.forEach(function (endpoint) {
        var sourceIndex = indexMap[endpoint.ip];
        Object.keys(endpoint.connections).forEach(function (ip) {
            var targetIndex = indexMap[ip];
            if (targetIndex !== null && targetIndex !== undefined) {
                graph.addLink(sourceIndex, targetIndex);
            }
        });

        indexMap[endpoint.ip] = null;
    });

    var layout = Viva.Graph.Layout.forceDirected(graph, {
        springLength : 80,
        springCoeff : 0.0005,
        dragCoeff : 0.02,
        gravity : -1.2,
        stableThreshold: 1
    });

    var graphics = Viva.Graph.View.webglGraphics();

    var renderer = Viva.Graph.View.renderer(graph, {
        graphics: graphics,
        layout: layout
    });
    var result = {nodes: nodes, links: links};

    console.log(result);

    renderer.run();

}
