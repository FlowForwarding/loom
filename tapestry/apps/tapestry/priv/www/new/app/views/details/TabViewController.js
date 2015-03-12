(function (angular) {
    'use strict';

    angular.module('nci.views.details.TabViewController', [])
        .controller("TabViewController", function($scope) {
            $scope.$on('$stateChangeSuccess', function(event, toState) {
                $scope.tabIndex = toState.data.index;
            });
        });

})(angular);
