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
            endpoints = sortEndpoints(NCI.model.endpoints(), "totalConnections", "desc"),
            d3Container = d3.select(container),
            activitiesListContainer = d3Container.select(".activities-list"),
            endpointsHistogramContainer = d3Container.select(".endpoints-histogram"),
            table = new NCI.Table(activitiesListContainer, columns, endpoints),
            histogram = new NCI.EndpointsHistogram(endpointsHistogramContainer, endpoints),
            topHundred = {
                name: "All Endpoints",
                endpoints: endpoints
            },
            breadcrumbsData = [topHundred],
            $histogramView = $container.find(".show-histogram"),
            $tableView = $container.find(".show-table"),
            $endpointSortMenu = $container.find(".endpoint-sort");


        $histogramView.on("click", function() {
            $histogramView.hide();
            $tableView.show();
            $(activitiesListContainer.node()).hide();
            $(endpointsHistogramContainer.node()).show();
        });

        $tableView.on("click", function() {
            $tableView.hide();
            $histogramView.show();
            $(activitiesListContainer.node()).show();
            $(endpointsHistogramContainer.node()).hide();
        });

        function sortEndpoints(endpoints, field, direction) {
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

        var $prevSort = $endpointSortMenu.find("[data-sort-field=totalConnections] [data-sort-direction=desc]");
        triggerActive($prevSort, true);

        $endpointSortMenu.on("click", "[data-sort-direction]", function() {
            var $el = $(this),
                sortDirection = $el.data("sortDirection"),
                sortField = $el.parents("[data-sort-field]").data("sortField"),
                endpoint = breadcrumbsData[breadcrumbsData.length - 1].endpoint,
                currentData = endpoint ?
                    Object.keys(endpoint.connections).map(function(key) {return endpoint.connections[key]}) :
                    endpoints;

            triggerActive($prevSort, false);
            triggerActive($el, true);

            $prevSort = $el;

            currentData = sortEndpoints(currentData, sortField, sortDirection);
            console.log("sorted?");
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
            setEndpoints(endpoints);

        }

        updateBreadcrumbs(breadcrumbsData);

        $([table, histogram]).on("click", function(event, data) {
            setActiveEndpoint(data);
        });

        $histogramView.click();

        // trigger resize here in order to adjust histogram height;
        // TODO: get rid of this hack
        $(window).resize();
    }

    NCI.EndpointsView = EndpointsView;
})();
