'use strict';

angular.module('nci.collectorsView', [
    'ngRoute',
    'smart-table'
    ])
    .config(['$routeProvider', function($routeProvider) {
        $routeProvider    
            .when('/collectors', {
                templateUrl: "./views/collectors/collectorsView.html",
                resolve: {
                    collectors: function($q, $timeout, connection) {
                       return connection().then(function(con) {
                           return con.getCollectors();
                       });
                    }
                },
                controller: 'CollectorsViewCtrl'
            });
    }])
    .controller('CollectorsViewCtrl', ['$scope', 'collectors', function($scope, collectors) {
        $scope.rowCollection = collectors;
    }]);
//    .directive("stFixedHeader", function() {
//        return {
//            restrict: "A",
//            transclude: true,
//            replace: true,
//            template: '<div><table class="table table-striped" ng-transclude></table></div>'
//        }
//    });

