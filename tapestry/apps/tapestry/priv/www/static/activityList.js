(function() {
    function createTable(columns, d3Selection, data) {
        var table = d3Selection.append("table")
                .attr("class", "columns"),
            thead = table.append("thead"),
            tbody = table.append("tbody");

        // append the header row
        thead.append("tr")
            .selectAll("th")
            .data(columns)
            .enter()
            .append("th")
            .text(function(column) { return column.text; });

        // apply data for table body
        updateTableBody(columns, tbody, data);

        return table;
    }

    function updateTableBody(columns, tbody, data) {
        var rows = tbody.selectAll("tr")
            .data(data, function(d) {return d.endpoint});


        var cells = rows.enter()
            .append("tr")
            .selectAll("td")
            .data(function(row) {
                return columns.map(function (column) {
                    return {column: column, value: row[column.property]};
                })
            });

        cells.enter()
            .append("td")
            .html(function(d) {return d.value});

        rows.exit()
            .remove();

        rows.order();
    }

    function downloadFile(fileType, fileContent, fileName) {
        $("<a>")
            .attr("href", encodeURI(fileType + fileContent))
            .attr("download", fileName)
            .get(0)
            .click();
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

    function parseActivity(activity) {
        function isOutside(endpoint) {
            return activity.Endpoints.indexOf(endpoint) < 0;
        }

        function createEndpoint(endpoint) {
            return {
                activity: activity.Label,
                endpoint: endpoint,
                totalConnections: 0,
                outsideConnections: 0,
                external: NCI.isExternal(endpoint)
            }
        }

        function getEndpoint(endpoint) {
            return endpoints[endpoint];
        }

        var endpoints = {};

        activity.Endpoints.forEach(function(endpoint) {
            endpoints[endpoint] = createEndpoint(endpoint);
        });

        activity.Interactions.forEach(
            function(interaction) {
                var outsideInteraction = false,
                    ep = null;

                interaction.forEach(function(endpoint) {
                    if (isOutside(endpoint)) {
                        outsideInteraction = true
                    } else {
                        ep = getEndpoint(endpoint);
                        ep.totalConnections += 1;
                    }
                });

                if (ep && outsideInteraction) {
                    ep.outsideConnections += 1;
                }

            }
        );

        return Object.keys(endpoints).map(function(endpoint) {return endpoints[endpoint]});
    }

    function parseActivities(activities) {
        return Array.prototype.concat.apply([], activities.map(parseActivity));
    }

    function sortActivities(activities) {
        return activities.sort(function(a1, a2) {
            if (a1.activity < a2.activity) {
                return -1;
            }
            if (a1.activity > a2.activity)
                return 1;
            return a2.totalConnections - a1.totalConnections;
        })
    }

    function getCommunitiesColumns(communities) {
        return communities.length > 1 ? activitiesColumns : activityColumns;
    }

    function getActivityName(communities) {
        return communities.length > 1 ? "activities" : (communities[0].Label);
    }

    function stringToRegex(str) {
        // converts string with wildcards to regex
        // * - zero or more
        // ? - exact one

        str = str.replace(/\./g, "\\.");
        str = str.replace(/\?/g, ".");
        str = str.replace(/\*/g, ".*");

        return new RegExp(str, "g");
    }

    var activityColumns = [
            {text: "Endpoint", property: "endpoint"},
            {text: "Connections", property: "totalConnections"},
            {text: "Outside Connections", property: "outsideConnections"},
            {text: "External", property: "external"}
        ],
        activitiesColumns = [
            {text: "Activity", property: "activity"},
            {text: "Endpoint", property: "endpoint"},
            {text: "Connections", property: "totalConnections"},
            {text: "Outside Connections", property: "outsideConnections"},
            {text: "External", property: "external"}
        ],
        downloadCSV = downloadFile.bind(null, "data:text/csv;charset=utf-8,");

    function ListBuilder(communities) {
        this.columns = getCommunitiesColumns(communities);
        this.activityName = getActivityName(communities);
        this.activities = sortActivities(parseActivities(communities));
        this.table = null;
    }

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

        this.table = createTable(columns, d3Selection, activities);
        return this.table;
    };

    ListBuilder.prototype.removeTable = function() {
        if (this.table) {
            this.table.remove();
        }
        this.table = null;
    };

    ListBuilder.prototype.filterTable = function(filterTerm) {
        var tbody = this.table.select("tbody"),
            re = stringToRegex(filterTerm),
            data = this.activities.filter(function(d, index) {
                return re.test(d.endpoint);
            })

        updateTableBody(this.columns, tbody, data);
    };

    NCI.list = {
        createListBuilder: function(communities) {
            return new ListBuilder(communities);
        }
    };

})();
