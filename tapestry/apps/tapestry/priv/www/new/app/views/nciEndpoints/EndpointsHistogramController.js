(function (angular) {
    'use strict';

    angular.module('nci.endpointsView.HistogramController', [])
        .controller('EndpointsHistogramController', [
            "$scope",
            function($scope) {
                function createTooltip(activity) {
                    return [
                        'Activity #' + activity.index,
                        'Size: ' + activity.size,
                        'Internal Flows: ' + activity.internalFlows,
                        'External Flows: ' + activity.externalFlows,
                        'Average Internal Flows: ' + activity.avgInternalFlows.toFixed(2)
                    ].join("\n");
                }

                var endpoints = $scope.endpoints,
                    details = endpoints.all().sort(function(e2, e1) {return e1.totalConnections - e2.totalConnections;}),
                    rows = details.map(function(endpoint) {
                        return {c: [
                            {v: endpoint.ip},
                            {v: endpoint.internalConnections},
                            {v: endpoint.externalConnections}
                            //{v: createTooltip(activity)},
                            //{v: maxActivities.has(activity) ? "#2ca02c" : null}
                        ]};
                    });


                $scope.chartObject = {};

                $scope.select = function(selection) {

                    console.log(selection);
                };

                $scope.rows = rows;

                $scope.page = 0;
                $scope.prevDisabled = true;
                $scope.itemsPerPage = 20;
                $scope.goToPage = function(page) {
                    $scope.prevDisabled = false;
                    $scope.nextDisabled = false;

                    if (page <= 0) {
                        $scope.prevDisabled = true;
                    }
                    if (Math.floor(rows.length/$scope.itemsPerPage) <= page) {
                        $scope.nextDisabled = true;
                    }
                    $scope.page = page;
                };

                $scope.chartObject.data = {"cols": [
                    {id: "t", label: "Label", type: "string"},
                    {id: "s", label: "Internal Connections", type: "number"},
                    {id: "s", label: "External Connections", type: "number"},
                    //{role: "tooltip", type: "string"},
                    //{role: "style", type: "string"}
                ], "rows": []};

                // $routeParams.chartType == BarChart or PieChart or ColumnChart...
                $scope.chartObject.type = "ColumnChart";
                $scope.chartObject.options = {
                    //'title': 'Activities distribution',
                    "isStacked": "true",
                    "legend": "none",
                    "vAxis": {
                        "labels": "false",
                        //"logScale": "true",
                        //"title": "Number of Endpoints per Activity X[j]"
                    },
                    "hAxis": {
                        "labels": "false",
                        //"title": ['Activities Sorted by Size ',
                        //    'NCI(N) = Max j, X[j] ≥ j ',
                        //    'where NCI(N) is the Network Complexity Index of network N and X[j] is the number of endpoints engaged in an activity'].join("\n")
                    }
                };
            }])
        .directive("chartPaging", function() {
            return {
                link: function($scope, $el, attributes) {
                    // TODO: fix this

                    function updateChartData() {
                        var currentPage = $scope.$eval(attributes.chartCureentPage),
                            itemsPerPage = $scope.$eval(attributes.chartItemsPerPage),
                            rows = $scope.$eval(attributes.chartPaging);

                        // TODO: throw?
                        if (currentPage < 0) {return;}
                        if (currentPage*itemsPerPage > rows.length) {return;}

                        $scope.chartObject.data.rows = rows.slice(currentPage * itemsPerPage, (currentPage + 1) * itemsPerPage);
                    }

                    $scope.$watch(attributes.chartCureentPage, function(page) {
                        updateChartData();
                    });
                }
            };
        });
})(angular);
