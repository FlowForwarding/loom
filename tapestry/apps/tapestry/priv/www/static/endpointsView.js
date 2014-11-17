(function() {

    var columns = [
        {text: "Endpoint", property: "ip", sort: null, filter: null},
        {text: "Internal", property: "internalConnections"},
        {text: "External", property: "externalConnections"},
        {text: "Total", property: "totalConnections", sort: NCI.Table.DESC_DIRECTION, filter: null},
        {
            text: "Activity",
            property: "activity",
            sort: null,
            filter: null,
            renderer: function(activity) {
                return activity ? "Activity #" + activity.index : "Activity not loaded";
            },
            sortFn: function(activity1, activity2) {
                return activity2.index - activity1.index;
            }
        },
//        {text: "Outside Connections", property: "outsideConnections", sort: null, filter: null},
        {property: "external", filter: null, hidden: false, text: "isExternal"}
    ];


    function EndpointsView($container) {
        NCI.model.parseActivities(NCI.Communities);

        var container = $container.get(0),
            $histogramView = $container.find(".show-histogram"),
            $tableView = $container.find(".show-table"),
            $graphView = $container.find(".show-graph"),
            $endpointSortMenu = $container.find(".endpoint-sort"),
            $prevSort = $(),
            endpoints = sortEndpoints(NCI.model.endpoints(), "totalConnections", "desc"),
            d3Container = d3.select(container),
            activitiesListContainer = d3Container.select(".activities-list"),
            endpointsHistogramContainer = d3Container.select(".endpoints-histogram"),
            endpointsGraphContainer = d3Container.select(".endpoints-graph"),
            table = new NCI.Table(activitiesListContainer, columns, []),
            histogram = new NCI.EndpointsHistogram(endpointsHistogramContainer, []),
            graph = new NCI.Graph(endpointsGraphContainer, []),
            isGraphActive = false,
            topHundred = {
                name: "All Endpoints",
                endpoints: endpoints
            },
            breadcrumbsData = [topHundred];

        this.stop = function() {
            graph.stop();
        };

        this.resume = function() {
            if (isGraphActive) {
                graph.resume();
            }
        };

        $histogramView.on("click", function() {
            $histogramView.hide();
            $tableView.show();
            $graphView.show();
            graph.stop();
            $(activitiesListContainer.node()).hide();
            $(endpointsGraphContainer.node()).hide();
            $(endpointsHistogramContainer.node()).show();
            $endpointSortMenu.show();
            // trigger resize here in order to adjust histogram height;
            // TODO: get rid of this hack
            $(window).resize();

            isGraphActive = false;
        });

        function setGraphViewActive() {
            $histogramView.show();
            $graphView.hide();
            $tableView.show();
            graph.resume();
            $(activitiesListContainer.node()).hide();
            $(endpointsHistogramContainer.node()).hide();
            $(endpointsGraphContainer.node()).show();
            $endpointSortMenu.hide();
            // trigger resize here in order to adjust histogram height;
            // TODO: get rid of this hack
            $(window).resize();

            isGraphActive = true;
        }

        $graphView.on("click", setGraphViewActive);

        $tableView.on("click", function() {
            $tableView.hide();
            $histogramView.show();
            $graphView.show();
            graph.stop();
            $(activitiesListContainer.node()).show();
            $(endpointsHistogramContainer.node()).hide();
            $(endpointsGraphContainer.node()).hide();
            $endpointSortMenu.hide();
            // trigger resize here in order to adjust histogram height;
            // TODO: get rid of this hack
            $(window).resize();

            isGraphActive = false;
        });

        function sortEndpoints(endpoints, field, direction) {
            var $fieldItem = $endpointSortMenu.find("[data-sort-field=" + field + "] [data-sort-direction=" + direction + "]");
            triggerActive($prevSort, false);
            triggerActive($fieldItem, true);

            $prevSort = $fieldItem;

            direction = direction === "desc" ? -1 : 1;
            return endpoints.sort(function(item1, item2) {
                var res = (item1[field] - item2[field])*direction;
                if (res===0) {
                    res = item1.ip > item2.ip ? 1 : -1;
                }
                return res;
            });
        }

        function triggerActive($el, shouldSet) {
            $el.parents("[data-sort-field]").toggleClass("active", shouldSet);
            $el.toggleClass("active", shouldSet);
        }

        $endpointSortMenu.on("click", "[data-sort-direction]", function() {
            var $el = $(this),
                sortDirection = $el.data("sortDirection"),
                sortField = $el.parents("[data-sort-field]").data("sortField"),
                endpoint = breadcrumbsData[breadcrumbsData.length - 1].endpoint,
                currentData = endpoint ?
                    Object.keys(endpoint.connections).map(function(key) {return endpoint.connections[key]}) :
                    endpoints;

            currentData = sortEndpoints(currentData, sortField, sortDirection);
            setEndpoints(currentData);
        });

        function updateBreadcrumbs() {
            var length = breadcrumbsData.length,
                breadcrumbs = d3Container.select(".breadcrumbs")
                    .selectAll("li")
                    .data(breadcrumbsData, function(d) {return d.name});

            breadcrumbs.enter()
                .append("li")
                .append("a")
                .html(function(d) {return d.name});

            breadcrumbs.exit()
                .remove();

            breadcrumbs.on(".click");

            breadcrumbs
                .classed("current", function(d, index) {
                    return (length - 1) == index;
                })
                .on("click", function(d, index) {
                    if (d !== topHundred) {
                        setActiveEndpoint(d.endpoint);
                    } else {
                        breadcrumbsData = [topHundred];
                        updateBreadcrumbs();
                        setEndpoints(topHundred.endpoints);
                    }
                });

            return breadcrumbs;
        }

        function setEndpoints(endpoints) {
            table.setData(endpoints);
            histogram.setData(endpoints);

            if (endpoints.length > NCI.max_vertices) {
                $graphView.parent("li").addClass("disabled");
                $graphView.off("click");

                if (isGraphActive) {
                    $histogramView.click();
                }

            } else {
                $graphView.parent("li").removeClass("disabled");
                $graphView.on("click", setGraphViewActive);
                graph.setData(endpoints);
            }

            if (isGraphActive) {
                graph.resume();
            }
        }

        function setActiveEndpoint(endpoint) {
            var endpoints = Object.keys(endpoint.connections).map(function(key) {return endpoint.connections[key]}),
                indexOfBreadcrumb = -1;

            breadcrumbsData.forEach(function(breadcrumb, index) {
                if (breadcrumb.name == endpoint.ip) {
                    indexOfBreadcrumb = index;
                    return false;
                }
            });

            if (indexOfBreadcrumb<0) {
                breadcrumbsData.push({
                    name: endpoint.ip,
                    endpoint: endpoint
                });
            } else {
                breadcrumbsData = breadcrumbsData.slice(0, indexOfBreadcrumb + 1);
            }

            updateBreadcrumbs();
            setEndpoints(sortEndpoints(endpoints, "totalConnections", "desc"));

        }

        updateBreadcrumbs(breadcrumbsData);

        $([table, histogram, graph]).on("click", function(event, data) {
            setActiveEndpoint(data);
            if (this==graph) {
                graph.resume();
            }
        });

        setEndpoints(endpoints);
        $histogramView.click();

    }

    NCI.EndpointsView = EndpointsView;
})();
