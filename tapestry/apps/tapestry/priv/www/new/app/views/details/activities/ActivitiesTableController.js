'use strict';

angular.module('nci.views.details.activities.ActivitiesTableController', [
        'nci.services.nciActivityModel'
    ])
    .controller('ActivitiesTableController', ["$scope", "activities", function($scope, activities) {
        $scope.rows = activities.all();
    }]);