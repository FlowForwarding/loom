(function (angular) {
    'use strict';

    angular.module('nci.services.export', [])
        .value("endpointsColumns", [
            {text: "Host", property: "host"},
            {text: "Endpoint", property: "ip"},
            {text: "Internal", property: "internalConnections"},
            {text: "External", property: "externalConnections"},
            {text: "Total", property: "totalConnections"},
            {text: "Activity", property: "activity", renderer:
                function(activity) {
                    return activity ? "Activity #" + activity.index : "NOT FOUND";
                }
            },
            {text: "OutsideConnections", property: "outsideConnections"},
            {text: "isExternal", property: "external"}

        ])
        .value("activitiesColumns", [])
        .factory("exportToCSV", function(createCSV, endpointsColumns, downloadFile) {
            return function(data, name) {
                downloadFile("text/csv", createCSV(endpointsColumns, data), name + ".csv");
            };
        })
        /** Example of columns definition
         * [
         *     {
         *          text: "Activity",
         *          property: "activity",
         *          renderer: function(value) {
         *              return "Activity #" + value
         *          }
         *     },
         * ]
         */
        .factory("createCSV", function() {
            return function createCSV(columns, data) {
                var csvRows = [columns.map(function(item) {
                    return ['"',
                        item.text.replace('"', '""'),
                        '"'].join("");
                }).join(",")];

                csvRows = csvRows.concat(data.map(function(item) {
                    return columns.map(function(column) {
                        var res = item[column.property];
                        if (column.renderer) {
                            res = column.renderer(res);
                        }
                        return res;
                    }).join(",");
                }));

                return csvRows.join("\n");
            };
        })
        .factory("downloadFile", function() {
            return function downloadFile(fileType, fileContent, fileName) {
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
            };
        });

})(angular);
