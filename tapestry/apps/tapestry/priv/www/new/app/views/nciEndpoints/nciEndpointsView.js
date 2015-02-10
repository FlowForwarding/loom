(function (angular) {
    'use strict';

    angular.module('nci.endpointsView', [
        'ngRoute',
        'nci.services.nciEndpointModel',
        'nci.activitiesView.HistogramController',
        'nci.endpointsView.GraphController'
    ])
        .config(['$routeProvider', function($routeProvider) {
            $routeProvider
                .when('/details/endpoints', {
                    templateUrl: "./views/nciEndpoints/nciEndpointsView.html",
                    resolve: {
                        endpoints: function(endpointsPromise) {
                            return endpointsPromise;
                        }
                    }
                });
        }])
        .controller('EndpointsTableController', ["$scope", "endpointsPromise", function($scope, endpointsPromise) {
            endpointsPromise
                .then(function(endpoints) {
                    $scope.rowCollection = endpoints.all();
                    //$scope.$apply();
                });
        }]);

})(angular);
