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
                templateUrl: "./views/nciActivites/nciActivitiesView.html",
                controller: "ActivitiesViewController",
                resolve: {
                    activities: function(activitiesPromise) {
                        return activitiesPromise;
                    }
                }
            });
    }])
    .controller('ActivitiesViewController', ["$scope", "activities", function($scope, activities) {
        $scope.activities = activities;
        $scope.rows = activities.all();
    }]);