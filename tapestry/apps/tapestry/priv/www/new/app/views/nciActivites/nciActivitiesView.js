'use strict';

angular.module('nci.activitiesView', [
        'ngRoute',
        'nci.services.nciActivityModel',
        'nci.activitiesView.HistogramController',
        'nci.activitiesView.GraphController'
    ])
    .config(['$routeProvider', function($routeProvider) {
        $routeProvider    
            .when('/details/activities', {
                templateUrl: "./views/nciMonitor/nciActivitiesView.html"
            });
    }])
    .controller('TableController', ["$scope", "activitiesPromise", function($scope, activitiesPromise) {
        activitiesPromise
            .then(function(activities) {
                $scope.rowCollection = activities.all();
                //$scope.$apply();
            });
    }]);