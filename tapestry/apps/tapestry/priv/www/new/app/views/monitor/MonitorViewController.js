'use strict';

angular.module('nci.views.monitor.MonitorViewController', [])
    .controller('MonitorViewController', ["$scope", "$filter", "con", function($scope, $filter, connection) {
        
        function formatDate(date) {
            return $filter('date')(date, "'yyyy-MMM-dd HH:mm:ss");
        }
        
        $scope.options = {
            labels : ['NCI', 'NCI'],
            zoomCallback: function(minDate, maxDate, yRanges) {
                $scope.$apply(function() {
                    $scope.currentPeriod = null;
                });
            },
            fillGraph: true,
            connectSeparatedPoints: true,
            yRangePad: 10,
            gridLineWidth: 0.1,
            axisLabelFontSize: 10,
            xAxisLabelWidth: 70,
            //logscale: true,
            axes: {
                x: {
                    axisLabelFormatter: formatDate,
                    valueFormatter: formatDate,
                    ticker : Dygraph.dateTicker,
                    pixelsPerLabel: 100
                },
                y: {
                    valueRange: [1, null]
                }
            },
            ylabel: 'NCI',
            labelsDivStyles: {
                'textAlign': 'right'
            },
            showRangeSelector: true
        };
        $scope.data = [];

        $scope.currentPeriod = null;
        $scope.range = null;

        $scope.updatePeriod = function(period) {
            var startDate = Date.now() - period * 24 * 60 * 60 * 1000;
            connection.getMore(startDate).then(function() {
                $scope.currentPeriod = period;
                $scope.range = [(new Date(startDate)).getTime(), Date.now()];
            });
        };
        
        $scope.buttons = [
            {text: '1d', period: 1},
            {text: '5d', period: 5},
            {text: '1m', period: 1 * 30},
            {text: '3m', period: 3 * 30},
            {text: '6m', period: 6 * 30},
            {text: '1y', period: 1 * 30 * 12}//,
//            {text: '5y', period: 5 * 30 * 12},
//            {text: '10y', period: 10 * 30 * 12}
        ];

        connection.getMore(0)
            .then(function() {
                var minDate = connection.nciData[0][0];

                $scope.range = [minDate, Date.now()];
                $scope.currentPeriod = null;
            });

        $scope.$watchCollection(function() {
            return connection.nciData;
        }, function(data) {
            $scope.data = data;
        });
    }])
    //TODO: move directive out of here
    .directive('nciDygraphs', ["$timeout", function($timeout) {
        return {
            restrict: 'E',
            scope: {
                data: '=',
                options: '=?',
                dateWindow: '=?range'
            },
//            template: "<div></div>",
            link: function(scope, elem, attrs) {
                $timeout(function() {
                    var graph = new Dygraph(elem[0], scope.data, scope.options);
                    scope.$watch("data", function() {
                        graph.updateOptions({
                            file: scope.data,
                            connectSeparatedPoints: true,
                            drawCallback: scope.drawCallback
                        });
                    }, true);
                    scope.$watch("dateWindow", function() {
                        graph.updateOptions({
                            dateWindow: scope.dateWindow
                        });
                    });
                    scope.drawCallback = function(data) {
//                    var xAxisRange = data.xAxisRange();
//                    if (!scope.view) {
//                        scope.view = {};
//                    }
//                    scope.view.from = xAxisRange[0];
//                    scope.view.to = xAxisRange[1];
//                    if (!scope.$root.$$phase) {
//                        scope.$apply();
//                    }
                    };
                }, 1);
            }
        };
    }]);

