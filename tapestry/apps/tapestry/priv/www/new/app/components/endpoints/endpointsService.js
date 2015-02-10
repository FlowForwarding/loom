(function (angular) {
    'use strict';

    angular.module('nci.components.services.endpointsService', [
        'ngMaterial',
        'nci.components.nciSigmaGraph'
    ])
        .controller("endpointsDialogController", function($scope, $mdDialog, sharedEndpointsHack, colorForActivity) {
            var edgesSet = new Set(),
                edges = [],
                endpointsSet = new Set(),
                endpoints = sharedEndpointsHack.endpoints;

            function createEdge(target, source, weight, size) {
                var id = target + "_" + source;
                if (!edgesSet.has(id) && endpointsSet.has(target) && endpointsSet.has(source)) {
                    edges.push({
                        id: id,
                        source: source,
                        target: target,
                        weight: weight,
                        size: size
                    });
                    edgesSet.add(id);
                    edgesSet.add(source + "_" + target);
                }
            }

            function createEndpointNode(endpoint) {
                endpointsSet.add(endpoint.ip);
                endpoint.getConnections().forEach(function(ep) {
                    createEdge(endpoint.ip, ep.ip, 0.2, 0.1);
                });

                return {
                    id: endpoint.ip,
                    size: 1,
                    label: endpoint.ip,
                    x: Math.random()*20 - 10,
                    y: Math.random()*20 - 10,
                    color: colorForActivity(endpoint.activity)
                };
            }

            $scope.nodes = endpoints.map(createEndpointNode);
            $scope.edges = edges;


            $scope.closeDialog = function() {
                $mdDialog.hide();
            };
        })
        .value("sharedEndpointsHack", {
            endpoints: null
        })
        .factory("nciEndpointsDialog", [
            "$mdDialog",
            "sharedEndpointsHack",
            function($mdDialog, sharedEndpointsHack) {
            return {
                show: function (endpoints) {
                    sharedEndpointsHack.endpoints = endpoints;
                    $mdDialog.show({
                        templateUrl: "./components/endpoints/endpoints-service.html",
                        controller: 'endpointsDialogController'
                    });
                }
            };
        }]);

})(angular);
