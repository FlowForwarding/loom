(function() {

    function createTable(columns, d3Selection, options) {
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

        options = options || {};

        var pagesToShow = options.pagesToShow || 1,
            itemsPerPage = options.itemsPerPage || 100;

        table.datum({
            columns: columns,
            pagesToShow: pagesToShow,
            itemsPerPage: itemsPerPage,
            data: []
        });

        table.append("tfoot")
            .append("tr")
            .append("td")
            .attr("colspan", columns.length)
            .html('<a href="#">Show next ' + itemsPerPage + ' records</a>')
            .on("click", function() {
                var config = table.datum();

                config.pagesToShow += 1;
                table.datum(config);

                updateTable(table, config.columns, config.data);
            });

        thead.append("tr");

        return container;
    }

    function sortData(data, column, direction, sortFn) {
        return data.sort(function(a1, a2) {
            return sortFn(a1[column], a2[column])*direction;
        })
    }

    function defaultSortFn(v1, v2) {
        var sort = 0;
        if (v1 < v2) {
            sort = -1;
        }
        if (v1 > v2) {
            sort = 1;
        }
        return sort;
    }

    function filterData(data, column, filter) {
        var re = stringToRegex(filter);

        return data.filter(function(d) {
            return re.test(d[column]);
        });
    }

    function updateTable(table, columns, data) {
        var thead = table.select("thead"),
            tfoot = table.select("tfoot"),
            currentData = data,
            config = table.datum(),
            pagesToShow,
            itemsPerPage = config.itemsPerPage,
            chunks = [],
            totalChunks,
            nextText = "Show next " + itemsPerPage + " rows. ";

        if (config.data!==data) {
            config.pagesToShow = 1;
        }

        pagesToShow = config.pagesToShow;

        for (var i=0; i < pagesToShow; i++) {
            chunks.push(currentData.slice(i*itemsPerPage, (i + 1)*itemsPerPage));
        }

        totalChunks = Math.ceil(data.length/itemsPerPage);

        config.data = data;
        table.datum(config);

        nextText += (data.length - pagesToShow*itemsPerPage) + " left";

        if (totalChunks - 1 == pagesToShow) {
            nextText = "Show last " + data.length%itemsPerPage + " rows";
        }

        tfoot.select("a")
            .text(nextText);

        tfoot.classed("hide", totalChunks==pagesToShow);

        updateTableHeader(columns, thead);

        table.on(".click");

        updateTableBody(columns, table, chunks);

        table.selectAll("tbody").selectAll("tr")
            .on("click", function(d) {
                $(table.node()).trigger("rowClick", d);
            });
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

    function getIdProperty(columns) {
        var property = columns[0].property;

        columns.forEach(function(column) {
            if (column.idProperty) {
                property = column.property;
                return false;
            }
        });

        return property;
    }

    function updateTableBody(columns, table, dataChunks) {
        var idProperty = getIdProperty(columns),
            chunks = table.selectAll("tbody")
                .data(dataChunks),
            chunksEnter = chunks.enter()
                .insert("tbody", "tfoot"),

            rows = chunks.selectAll("tr")
                .data(function(chunk) {
                    return chunk;
                }, function(d) {return d[idProperty]});

        chunks.exit()
            .remove()

        var cells = rows.enter()
            .append("tr")
            .classed("external-endpoint-row", function(d) {
                // TODO: handle this correctly
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

    function updateFilter(columns, property, filter) {
        columns.forEach(function(column) {
            if (column.property === property) {
                column.filter = filter;
            }
        });
    }

    function handleHeaderClick(table) {
        return function(data) {
            var $el = $(this),
                field = data.property,
                direction = $el.hasClass("desc") ? ASC_DIRECTION : DESC_DIRECTION;

            table.sort(field, direction);
        }
    }

/** Example of columns definition
 * [
 *     {text: "Endpoint", property: "endpoint", sort: null, filter: null},
 *     {text: "Internal", property: "internalConnections"},
 *     {text: "External", property: "externalConnections"},
 *     {text: "Total", property: "totalConnections", sort: DESC_DIRECTION, filter: null},
 *     {
 *          text: "Activity",
 *          property: "activity",
 *          sort: null,
 *          filter: null,
 *          sortFn: function(value1, value2) {
 *              return value1.name - value2.name
 *          },
 *          renderer: function(value) {
 *              return "Activity #" + value
 *          }
 *     },
 *     {text: "Outside Connections", property: "outsideConnections", sort: null, filter: null},
 *     {property: "external", filter: "false", hidden: true, text: "isExternal"}
 * ]
*/
    function getTable(container) {
        return container.select("table");
    }

    // container is d3Selection
    function Table(container, columns, data) {
        this.container = createTable(columns, container);
        this.table = getTable(this.container);

        var $me = $(this);

        $(this.table.node()).on("rowClick", function(event, data) {
            $me.trigger("click", data);
        });

        this.columns = columns;

        this.setData(data || null);

        this.table.selectAll(".th-inner").on("click", handleHeaderClick(this));
    }

    var ASC_DIRECTION = 1,
        DESC_DIRECTION = -1;

    Table.ASC_DIRECTION = ASC_DIRECTION;
    Table.DESC_DIRECTION = DESC_DIRECTION;

    Table.prototype.setData = function(data) {
        this.data = data;
        this._update();
    };


    Table.prototype.filter = function(field, filter) {
        updateFilter(this.columns, field, filter);
        this._update();
    };

    Table.prototype._update = function() {
        var currentData = this.data.slice(),
            columns = this.columns;

        columns.forEach(function(column) {
            if (column.filter) {
                currentData = filterData(currentData, column.property, column.filter);
            }
        });

        columns.forEach(function(column) {
            var sortFn = column.sortFn || defaultSortFn;
            if (column.sort) {
                currentData = sortData(currentData, column.property, column.sort, sortFn);
            }
        });

        updateTable(this.table, this.columns, currentData);
    };

    Table.prototype.sort = function(field, direction) {
        this.columns.forEach(function(column) {
            column.sort = null;
            if (column.property === field) {
                column.sort = direction;
            }
        });

        this._update();
    };

    Table.prototype.remove = function() {
        if (this.container) {
            this.container.on(".click");
            this.container.remove();
        }
        this.container = null;
        this.table = null;
        this.columns = null;
    };

    NCI.Table = Table;

})();

