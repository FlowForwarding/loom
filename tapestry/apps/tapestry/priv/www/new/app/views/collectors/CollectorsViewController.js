'use strict';

angular.module('nci.views.collectors.CollectorsViewController', [
        'smart-table'
    ])
    .controller('CollectorsViewController', ['$scope', 'collectors', function($scope, collectors) {
        $scope.rowCollection = collectors;
    }]);
