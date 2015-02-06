'use strict';

angular.module('nci.detailsView', [
        'ngRoute',
        'nci.services.nciActivityModel',
        'nci.detailsView.HistogramController',
        'nci.detailsView.GraphController'
    ])
    .config(['$routeProvider', function($routeProvider) {
        $routeProvider    
            .when('/details/:view', {
                templateUrl: "./views/nciDetails/nciDetailsView.html",
                resolve: {
                    activities: function($q, $timeout, activitiesPromise) {
                        return activitiesPromise;
                    }
                }
            });
    }])
    .controller('TableController', ["$scope", "activitiesPromise", function($scope, activitiesPromise) {
        activitiesPromise
            .then(function(activities) {
                $scope.rowCollection = activities.all();
                //$scope.$apply();
            });
    }]);