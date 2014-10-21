(function() {

    function createTable(columns, d3Selection, data) {
        var container = d3Selection
                .append("div")
                .classed("fixed-table-container", true)
                .classed("columns", true),
            background = container
                .append("div")
                .classed("header-background", true),
            table = container
                .append("div").classed("fixed-table-container-inner", true)
                .append("table"),

            thead = table.append("thead"),
            tbody = table.append("tbody");

        thead.append("tr");

        updateTable(table, columns, data);

        return container;
    }

    function sortActivities(activities, column, direction) {
        return activities.sort(function(a1, a2) {
            var sort = 0;
            if (a1[column] < a2[column]) {
                sort = -1;
            }
            if (a1[column] > a2[column]) {
                sort = 1;
            }
            return sort*direction;
        })
    }

    function filterActivities(activities, column, filter) {
        var re = stringToRegex(filter);

        return activities.filter(function(d) {
            return re.test(d[column]);
        });
    }

    function updateTable(table, columns, data) {
        var thead = table.select("thead"),
            tbody = table.select("tbody"),
            currentData = data;

        columns.forEach(function(column) {
            if (column.filter) {
                currentData = filterActivities(currentData, column.property, column.filter);
            }
        });

        columns.forEach(function(column) {
            if (column.sort) {
                currentData = sortActivities(currentData, column.property, column.sort);
            }
        });

        updateTableHeader(columns, thead);
        updateTableBody(columns, tbody, currentData);
    }

    function updateTableHeader(columns, thead) {
        var th = thead.select("tr")
            .selectAll("th")
            .data(columns.filter(function(d) {return !d.hidden})),

            thEnter = th.enter()
                .append("th");

        thEnter.append("span")
            .classed("th-container", true)
            .text(function(column) { return column.text; });

        thEnter.append("div")
            .classed("th-inner", true)
            .text(function(column) { return column.text; });

        th.selectAll(".th-inner")
            .classed("sorted", function(d) {
                return d.sort;
            })
            .classed("desc", function(d) {
                return d.sort === DESC_DIRECTION;
            });
    }

    function updateTableBody(columns, tbody, data) {
        var rows = tbody.selectAll("tr")
            .data(data, function(d) {return d.endpoint});


        var cells = rows.enter()
            .append("tr")
            .classed("external-endpoint-row", function(d) {
                return d.external;
            })
            .selectAll("td")
            .data(function(row) {
                var rowData = [];
                columns.forEach(function (column) {
                    var renderer = column.renderer || defaultRenderer;
                    if (!column.hidden) {
                        rowData.push({column: column, value: renderer(row[column.property])});
                    }
                });
                return rowData;
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

    function stringToRegex(str) {
        // converts string with wildcards to regex
        // * - zero or more
        // ? - exact one

        str = str.replace(/\./g, "\\.");
        str = str.replace(/\?/g, ".");
        str = str.replace(/\*/g, ".*");

        return new RegExp(str);
    }

    function defaultRenderer(value) {
        return value;
    }

    function createActivityColumns() {
        return [
            {text: "Endpoint", property: "endpoint", sort: null, filter: null},
            {text: "Internal", property: "internalConnections"},
            {text: "External", property: "externalConnections"},
            {text: "Total", property: "totalConnections", sort: DESC_DIRECTION, filter: null},
            {text: "Outside Connections", property: "outsideConnections", sort: null, filter: null},
            {property: "external", sort: null, filter: "false", hidden: true}
        ];
    }

    function createActivitiesColumns() {
        return [
            {text: "Endpoint", property: "endpoint", sort: null, filter: null},
            {text: "Internal", property: "internalConnections"},
            {text: "External", property: "externalConnections"},
            {text: "Total", property: "totalConnections", sort: DESC_DIRECTION, filter: null},
            {text: "Activity", property: "activity", sort: null,
                filter: null, renderer: function(value) {return "Activity #" + value}},
            {text: "Outside Connections", property: "outsideConnections", sort: null, filter: null},
            {property: "external", filter: "false", hidden: true}
        ]
    }

    function handleHeaderClick(listBuilder) {
        return function(data) {
            var $el = $(this),
                field = data.property,
                direction = $el.hasClass("desc") ? ASC_DIRECTION : DESC_DIRECTION;

            listBuilder.sortTable(field, direction);
        }
    }

    function ListBuilder(communities) {
        this.columns = getCommunitiesColumns(communities);
        this.activityName = getActivityName(communities);
        this.activities = parseActivities(communities);
        this.container = null;
    }

    function updateFilter(columns, property, filter) {
        columns.forEach(function(column) {
            if (column.property === property) {
                column.filter = filter;
            }
        });
    }

    var ASC_DIRECTION = 1,
        DESC_DIRECTION = -1,
        downloadCSV = downloadFile.bind(null, "data:text/csv;charset=utf-8,");

    ListBuilder.prototype.downloadCSV = function() {
        var columns = this.columns,
            csvName = this.activityName + ".csv",
            activities = this.activities,
            csvContent = createCSV(columns, activities);

        return downloadCSV(csvContent, csvName);
    };

    ListBuilder.prototype.createTable = function(d3Selection) {
        var columns = this.columns,
            activities = this.activities,
            container = createTable(columns, d3Selection, activities);

        container.selectAll(".th-inner").on("click", handleHeaderClick(this));

        this.container = container;

        return this.container;
    };

    ListBuilder.prototype.removeTable = function() {
        if (this.container) {
            this.container.on(".click");
            this.container.remove();
        }
        this.container = null;
    };

    ListBuilder.prototype.getTable = function() {
        return this.container.select("table");
    };

    ListBuilder.prototype.sortTable = function(field, direction) {
        var table = this.getTable();

        this.columns.forEach(function(column) {
            column.sort = null;
            if (column.property === field) {
                column.sort = direction;
            }
        });

        updateTable(table, this.columns, this.activities);
    };

    ListBuilder.prototype.filterTableByInternal = function(show) {
        // if show == true we need to show all endpoints
        // if show == false we need to show only internal endpoints (endpoint.external==false)
        var table = this.getTable();

        updateFilter(this.columns, "external", show ? null : "false");
        updateTable(table, this.columns, this.activities);
    };

    ListBuilder.prototype.filterTableByEndpoint = function(filterTerm) {
        var table = this.getTable();

        updateFilter(this.columns, "endpoint", filterTerm);
        updateTable(table, this.columns, this.activities);
    };

    NCI.list = {
        createListBuilder: function(communities) {
            return new ListBuilder(communities);
        }
    };

})();
