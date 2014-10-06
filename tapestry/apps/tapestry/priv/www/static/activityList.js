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

        // create a row for each object in the data
        var rows = tbody.selectAll("tr")
            .data(data)
            .enter()
            .append("tr");

        // create a cell in each row for each column
        var cells = rows.selectAll("td")
            .data(function(row) {
                return columns.map(function(column) {
                    return {column: column, value: row[column.property]};
                });
            })
            .enter()
            .append("td")
            .attr("style", "font-family: Courier")
            .html(function(d) { return d.value; });

        return table;
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

    function getCommunitiesColumns(communities) {
        return communities.length > 1 ? activitiesColumns : activityColumns;
    }

    function getActivityName(communities) {
        return communities.length > 1 ? "activities" : (communities[0].Label);
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
        this.activities = parseActivities(communities);
    }

    ListBuilder.prototype.downloadCSV = function() {
        var columns = this.columns,
            csvName = this.activityName + ".csv",
            activities = this.activities,
            csvContent = createCSV(columns, activities);

        return downloadCSV(csvContent, csvName);
    }

    ListBuilder.prototype.createTable = function (d3Selection) {
        var columns = this.columns,
            activities = this.activities;

        return createTable(columns, d3Selection, activities);
    }

    NCI.list = {
        createListBuilder: function(communities) {
            return new ListBuilder(communities);
        }
    };

})();
