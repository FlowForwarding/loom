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

        } else {
            this.force = d3.layout.force()
                .charge(-120)
                .linkDistance(30)
        }

        this.force
            .nodes(data.nodes)
            .links(data.links)
            .size([width, height]);

        var link = graph.selectAll(".link")
            .data(data.links, function(d) {return d.index});

        link.enter()
            .insert("line", ":first-child")
            .attr("class", "link")
            .style("stroke-width", function(d) { return Math.sqrt(d.value); });

        link.exit()
            .remove();

        var node = graph.selectAll(".node")
            .data(data.nodes, function(d) {return d.name});


        node.enter()
            .append("circle")
            .attr("class", "node")
            .attr("r", 5)
            .style("fill", function(d) { return color(d.group); })
            .on("click", function(data) {
                if (d3.event.defaultPrevented) return;
                $(graph.node()).trigger("nodeClick", data);
            })
            .call(this.force.drag)
            .append("title")
            .text(function(d) { return d.name; });

        node.exit()
            .remove();

        this.force
            .start()
            .stop();

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
        var currentData = buildGraphData(data, this.data);

        this.data = currentData;
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

    function buildGraphData(data, currentData) {
        var nodes = [],
            links = [],
            indexMap = {},
            currentNodesMap = {},
            currentLinksMap = {};

        if (currentData) {
            currentData.nodes.forEach(function(node) {
                currentNodesMap[node.name] = node;
            });
            currentData.links.forEach(function (link) {
                currentLinksMap[link.hash] = link;
            });
        }
        data.forEach(function (endpoint, index) {
            indexMap[endpoint.ip] = index;
            if (endpoint.ip in currentNodesMap) {
                nodes.push(currentNodesMap[endpoint.ip]);
            } else {
                nodes.push({
                    name: endpoint.ip,
                    group: endpoint.activity ? endpoint.activity.index : 0
                });
            }
        });

        data.forEach(function (endpoint) {
            var sourceIndex = indexMap[endpoint.ip];
            Object.keys(endpoint.connections).forEach(function (ip) {
                var targetIndex = indexMap[ip],
                    hash,
                    link;
                if (targetIndex !== null && targetIndex !== undefined) {
                    hash = [endpoint.ip, ip].join("|");

                    if (hash in currentLinksMap) {
                        link = currentLinksMap[hash];
                        links.push(link);
                    } else {
                        links.push({
                            source: sourceIndex,
                            target: targetIndex,
                            value: 1,
                            hash: hash,
                            index: [sourceIndex, targetIndex].join("|")
                        });
                    }
                }
            });

            indexMap[endpoint.ip] = null;
        });

        return {nodes: nodes, links: links};
    }


    NCI.Graph = Graph;
})();
