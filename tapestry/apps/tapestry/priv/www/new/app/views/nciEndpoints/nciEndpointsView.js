(function (angular) {
    'use strict';

    angular.module('nci.endpointsView', [
        'ngRoute',
        'nci.services.nciEndpointModel',
        'nci.endpointsView.HistogramController',
        'nci.endpointsView.GraphController'
    ])
        .config(['$routeProvider', function($routeProvider) {
            $routeProvider
                .when('/details/endpoints', {
                    templateUrl: "./views/nciEndpoints/nciEndpointsView.html",
                    controller: "EndpointsViewController",
                    resolve: {
                        endpoints: function(endpointsPromise) {
                            return endpointsPromise;
                        }
                    }
                });
        }])
        .controller('EndpointsViewController', ["$scope", "endpoints", "$rootScope", function($scope, endpoints, $rootScope) {
            $scope.endpoints = endpoints;
            $scope.rows = endpoints.all();

            $scope.$on("app:preferencesChanged", function(event, prefs) {
                $scope.showDomainNames = prefs.showDomainNames;
            });

        }]);

})(angular);
