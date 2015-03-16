(function (angular) {
    'use strict';

    angular.module('nci.views.details.endpoints.EndpointsTableController', [
        "smart-table"
    ])
        .controller('EndpointsTableController', [
            "$scope",
            "endpoints",
            "$state",
        function($scope, endpoints, $state) {
            $scope.rows = endpoints;

            $scope.$on("app:preferencesChanged", function(event, prefs) {
                $scope.showDomainNames = prefs.showDomainNames;
            });

            $scope.rowClick = function(endpoint) {
                $state.go($state.current, {
                    endpoint: endpoint.ip
                });
            };
        }]);

})(angular);
