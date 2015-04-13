(function (angular) {
    'use strict';

    angular.module('nci.views.details.endpoints.EndpointsTableController', [
        "smart-table"
    ])
        .controller('EndpointsTableController', [
            "$scope",
            "endpoints",
            "$state",
            "preferences",
        function($scope, endpoints, $state, preferences) {
            $scope.rows = endpoints;

            $scope.showDomainNames = preferences.showDomainNames;

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
