(function (angular) {
    'use strict';

    angular.module('nci.components.services.endpointsService', [
        'ngMaterial',
        'nci.components.nciSigmaGraph'
    ])
        .controller("endpointsDialogController", function($scope, $mdDialog, activity, colorForActivity) {
            var edgesSet = new Set(),
                edges = [],
                endpointsSet = new Set(),
                endpoints = activity.getEndpoints(),
                externalColor = "#69456f",
                outsideColor = "red";

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

            function isOutside(endpoint) {
                return endpoint.activity != activity;
            }

            function updateNodeColor(node) {
                var endpoint = node.endpoint,
                    showExternal = $scope.showExternal,
                    color = isOutside(endpoint) ? outsideColor : colorForActivity(endpoint.activity);


                node.color = showExternal && endpoint.external ? externalColor : color;
                return node;
            }

            $scope.updateDisplay = function() {
                var anchor = $scope.nodes.pop();
                $scope.nodes = $scope.nodes.map(updateNodeColor);
                $scope.nodes.push(anchor);
                $scope.updateGraph();
            };

            function createEndpointNode(endpoint) {
                endpointsSet.add(endpoint.ip);
                endpoint.getConnections().forEach(function(ep) {
                    createEdge(endpoint.ip, ep.ip, 0.2, 0.1);
                });

                return {
                    id: endpoint.ip,
                    endpoint: endpoint,
                    //outside:
                    size: 1,
                    label: endpoint.ip,
                    x: Math.random()*20 - 10,
                    y: Math.random()*20 - 10,
                    color: colorForActivity(endpoint.activity)
                };
            }

            $scope.closeDialog = function() {
                $mdDialog.hide();
            };

            var initialEndpoints = activity.getEndpoints(),
                endpointsToDisplay = new Set(initialEndpoints);

            initialEndpoints.forEach(function(endpoint) {
                endpoint.getConnections().forEach(function(connection) {
                    endpointsToDisplay.add(connection);
                });
            });

            $scope.nodes = [];

            endpointsToDisplay.forEach(function(endpoint) {
                $scope.nodes.push(updateNodeColor(createEndpointNode(endpoint)));
            });

            $scope.edges = edges;

            var anchor = {
                id: "anchor",
                size: 5,
                weight: 0,
                x: 0,
                y: 0,
                color: "transparent"
            };
            $scope.nodes.push(anchor);



        })
        .factory("nciEndpointsDialog", [
            "$mdDialog",
            function($mdDialog) {
            return {
                show: function (activity) {
                    $mdDialog.show({
                        templateUrl: "./components/endpoints/endpoints-service.html",
                        controller: 'endpointsDialogController',
                        locals: {
                            activity: activity
                        }
                    });
                }
            };
        }]);

})(angular);
