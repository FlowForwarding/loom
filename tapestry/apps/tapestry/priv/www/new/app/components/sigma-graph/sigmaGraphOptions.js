(function (angular) {
    'use strict';

    angular.module('sigmaGraphOptions', [])
        .value("forceLayoutConfig", {
                barnesHutTheta: 0.5,
                gravity: 0.3,
                strongGravityMode: true,
                adjustSizes: true,
                edgeWeightInfluence: 0.2,
                slowDown: 1,
                outboundAttractionDistribution: false
        })
        .controller("ForceLayoutConfigController", function($scope, forceLayoutConfig, $rootScope) {
            $scope.config = forceLayoutConfig;

            $scope.$watch("config", function() {
                $rootScope.$broadcast("graph:layout-update");
            }, true);
        });

})(angular);
