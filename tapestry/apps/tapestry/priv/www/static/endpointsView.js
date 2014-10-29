(function() {

    var columns = [
        {text: "Endpoint", property: "ip", sort: null, filter: null},
//        {text: "Activity", property: "activity", sort: null, filter: null, renderer: function(activity) {
//            return activity ? "Activity #" + activity.name : "Activity not loaded";
//        }},
        {text: "Total", property: "totalConnections", sort: NCI.Table.DESC_DIRECTION, filter: null},
    ];


    function EndpointsView($container) {
        console.time("parsing activities");
        NCI.model.parseActivities(NCI.Communities);
        console.timeEnd("parsing activities");

        var container = $container.get(0),
            endpoints = NCI.model.endpoints(),
            d3Container = d3.select(container),
            activitiesListContainer = d3Container.select(".activities-list"),
            table = new NCI.Table(activitiesListContainer, columns, endpoints),
            topHundred = {
                name: "All Endpoints",
                connections: endpoints
            },
            breadcrumbsData = [topHundred];

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
                    if (d!== topHundred) {
                        setActiveEndpoint(d.endpoint);
                    } else {
                        breadcrumbsData = [topHundred];
                        updateBreadcrumbs();
                        table.setData(endpoints);
                    }

                });

            return breadcrumbs;
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

            table.setData(endpoints);

        }

        updateBreadcrumbs(breadcrumbsData);

        $(table).on("click", function(event, data) {
            setActiveEndpoint(data);
        });
    }

    NCI.EndpointsView = EndpointsView;
})();
