'use strict';

angular.module('nci.monitor.monitor-directive', [])
    .controller("nciMonitorController", ["$scope", "$interval", "connection", function($scope, $interval, connection) {

        function createWatcher(object, field) {
            return function() {
                return object[field];
            };
        }

        connection().then(function(con) {
            $scope.$watch(createWatcher(con, 'nci'), function(nci) {
                $scope.nci = nci;
            });
            $scope.$watch(createWatcher(con, 'lastUpdatedNCI'), function(updatedAt) {
                $scope.updatedAt = updatedAt;
            });
            $scope.$watch(createWatcher(con, 'collectors'), function(collectors) {
                $scope.collectors = collectors;
            });
            $scope.$watch(createWatcher(con, 'ne'), function(ne) {
                $scope.flows = ne;
            });
            $scope.$watch(createWatcher(con, 'nep'), function(nep) {
                $scope.endpoints = nep;
            });
            $scope.$watch(createWatcher(con, 'qps'), function(qps) {
                $scope.flowsPerSecond = qps;
            });
        });
    }])
    .directive("nciMonitor", [function() {
        return {
            templateUrl: 'components/monitor/nci-monitor.html',
            controller: 'nciMonitorController',
            restrict: "E",
            scope: {}
        };
    }]);
