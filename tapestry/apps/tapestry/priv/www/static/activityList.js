(function() {

    function downloadFile(fileType, fileContent, fileName) {
        var blob = new Blob([fileContent], {type: fileType}),
            url = URL.createObjectURL(blob),
            $a = $("<a>")
                    .attr("href", url)
                    .attr("download", fileName)
                    .attr("target", "_blank");

        // This timeout is to fix issue with Safari download
        setTimeout(function() {
            $a
                .get(0)
                .click();
            // need timeout to have time to open file before we revoke it from memory
            setTimeout(URL.revokeObjectURL.bind(URL, url), 100);
        }, 0);
    }

    function createCSV(columns, data) {
        var csvRows = [columns.map(function(item) {
                return ['"',
                        item.text.replace('"', '""'),
                        '"'].join("");
            }).join(",")];

        csvRows = csvRows.concat(data.map(function(item) {
            return columns.map(function(column) {
                return item[column.property];
            }).join(",");
        }));

        return csvRows.join("\n");
    }

    function parseActivity(activity, index, activities) {
        var endpoints = {};
        function isOutside(endpoint) {
            return endpoint in endpoints;
        }

        function createEndpoint(endpoint) {
            return {
                activity: activities.length - index,
                endpoint: endpoint,
                internalConnections: 0,
                externalConnections: 0,
                totalConnections: 0,
                outsideConnections: 0,
                external: NCI.isExternal(endpoint)
            }
        }

        function getEndpoint(endpoint) {
            return endpoints[endpoint];
        }


        activity.Endpoints.forEach(function(endpoint) {
            endpoints[endpoint] = createEndpoint(endpoint);
        });
        function updateInteractionConnections(epA, epB) {
            var endpoint = getEndpoint(epA),
                outside = isOutside(epB),
                external = NCI.isExternal(epB);

            if (endpoint) {
                if (outside) {
                    endpoint.outsideConnections += 1;
                }
                endpoint[external ? "externalConnections" : "internalConnections"] += 1;
                endpoint.totalConnections += 1;
            }
        }

        activity.Interactions.forEach(
            function(interaction) {
                var endpointA = interaction[0],
                    endpointB = interaction[1];

                updateInteractionConnections(endpointA, endpointB);
                updateInteractionConnections(endpointB, endpointA);
            }
        );

        return Object.keys(endpoints).map(function(endpoint) {return endpoints[endpoint]});
    }

    function parseActivities(activities) {
        return Array.prototype.concat.apply([], activities.map(parseActivity));
    }

    function getCommunitiesColumns(communities) {
        return communities.length > 1 ? createActivitiesColumns() : createActivityColumns();
    }

    function getActivityName(communities) {
        return communities.length > 1 ? "activities" : (communities[0].Label);
    }

    function createActivityColumns() {
        return [
            {text: "Endpoint", property: "endpoint", sort: null, filter: null},
            {text: "Internal", property: "internalConnections"},
            {text: "External", property: "externalConnections"},
            {text: "Total", property: "totalConnections", sort: NCI.Table.DESC_DIRECTION, filter: null},
            {text: "Outside Connections", property: "outsideConnections", sort: null, filter: null},
            {property: "external", sort: null, filter: "false", hidden: true, text: "isExternal"}
        ];
    }

    function createActivitiesColumns() {
        return [
            {text: "Endpoint", property: "endpoint", sort: null, filter: null},
            {text: "Internal", property: "internalConnections"},
            {text: "External", property: "externalConnections"},
            {text: "Total", property: "totalConnections", sort: NCI.Table.DESC_DIRECTION, filter: null},
            {text: "Activity", property: "activity", sort: null,
                filter: null, renderer: function(value) {return "Activity #" + value}},
            {text: "Outside Connections", property: "outsideConnections", sort: null, filter: null},
            {property: "external", filter: "false", hidden: true, text: "isExternal"}
        ]
    }

    function ListBuilder(communities) {
        this.columns = getCommunitiesColumns(communities);
        this.activityName = getActivityName(communities);
        this.activities = parseActivities(communities);

        this.table = null;
    }


    var downloadCSV = downloadFile.bind(null, "text/csv");

    ListBuilder.prototype.downloadCSV = function() {
        var columns = this.columns,
            csvName = this.activityName + ".csv",
            activities = this.activities,
            csvContent = createCSV(columns, activities);

        return downloadCSV(csvContent, csvName);
    };

    ListBuilder.prototype.createTable = function(d3Selection) {
        var columns = this.columns,
            activities = this.activities;

        this.table = new NCI.Table(d3Selection, columns, activities);
    };

    ListBuilder.prototype.removeTable = function() {
        if (this.table) {
            this.table.remove();
        }
        this.table = null;
    };

    ListBuilder.prototype.filterTableByInternal = function(show) {
        // if show == true we need to show all endpoints
        // if show == false we need to show only internal endpoints (endpoint.external==false)
        var table = this.table;

        table.filter("external", show ? null : "false");
    };

    ListBuilder.prototype.filterTableByEndpoint = function(filterTerm) {
        var table = this.table;

        table.filter("endpoint", filterTerm);
    };

    NCI.list = {
        createListBuilder: function(communities) {
            return new ListBuilder(communities);
        }
    };

})();