(function (angular) {
    'use strict';

    angular.module('nci.views.details.endpoints.EndpointsTableController', [
        "smart-table"
    ])
        .controller('EndpointsTableController', ["$scope", "endpoints", "$rootScope", function($scope, endpoints) {
            $scope.rows = endpoints.all();

            $scope.$on("app:preferencesChanged", function(event, prefs) {
                $scope.showDomainNames = prefs.showDomainNames;
            });

        }]);

})(angular);
