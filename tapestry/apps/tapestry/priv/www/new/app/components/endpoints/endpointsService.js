(function (angular) {
    'use strict';

    angular.module('nci.components.services.endpointsService', [
        'ngMaterial',
        'nci.services.export',
        'nci.components.nciSigmaGraph'
    ])
        .controller("endpointsDialogController", function($scope, $mdDialog, activity, colorForActivity, exportToCSV, endpointTooltip, colors) {
            var edgesSet = new Set(),
                edges = [],
                nodes = [],
                insideNodes = [],
                insideEdges = [],
                endpointsSet = new Set();

            function createEdge(target, source, weight, size, isOutside) {
                var id = target + "_" + source;
                isOutside = isOutside || false;

                if (!edgesSet.has(id) && endpointsSet.has(target) && endpointsSet.has(source)) {
                    var edge = {
                        id: id,
                        source: source,
                        target: target,
                        weight: weight,
                        size: size
                    };
                    edges.push(edge);
                    if (!isOutside) {
                        insideEdges.push(edge);
                    }
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
                    color = isOutside(endpoint) ? colors.endpoints.OUTSIDE_ACTIVITY : colorForActivity(endpoint.activity);


                node.color = showExternal && endpoint.external ? colors.endpoints.EXTERNAL : color;
                return node;
            }

            $scope.activity = "Activity #" + activity.index;

            $scope.config = {
                redraw: 0
            };

            function updateGraph() {
                $scope.config.redraw++;
            }

            function updateOutsideDisplay() {
                var showOutside = $scope.showOutside;
                $scope.nodes = showOutside ? nodes : insideNodes;
                $scope.edges = showOutside ? edges : insideEdges;

            }

            function updateExternalDisplay() {
                $scope.nodes = nodes.map(updateNodeColor);
            }

            $scope.updateDisplay = function() {
                removeAnchor();
                updateExternalDisplay();
                updateOutsideDisplay();
                addAnchor();

                updateGraph();
            };


            function createEndpointNode(endpoint) {
                endpointsSet.add(endpoint.ip);
                var outside = isOutside(endpoint);
                endpoint.getConnections().forEach(function(ep) {
                    createEdge(endpoint.ip, ep.ip, 0.2, 0.1, isOutside(ep) || outside);
                });

                return {
                    id: endpoint.ip,
                    endpoint: endpoint,
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

            endpointsToDisplay.forEach(function(endpoint) {
                var node = updateNodeColor(createEndpointNode(endpoint));
                nodes.push(node);
                if (!isOutside(endpoint)) {
                    insideNodes.push(node);
                }
            });


            var anchor = {
                id: "anchor",
                size: 2,
                weight: 0,
                x: 0,
                y: 0,
                color: "transparent"
            };

            function removeAnchor() {
                nodes.pop();
                insideNodes.pop();
            }

            function addAnchor() {
                nodes.push(anchor);
                insideNodes.push(anchor);
            }

            addAnchor();
            updateOutsideDisplay();

            $scope.exportActivity = function() {
                var endpoints = [];

                endpointsToDisplay.forEach(function(ep) {
                    endpoints.push(ep);
                });

                exportToCSV(endpoints, activity.mainEndpoint.ip);
            };

            $scope.tooltip = function(node) {
                return endpointTooltip(node.endpoint);
            };

            $scope.legendKeys = [{
                shape: "circle",
                text: "endpoint in activity",
                color: colorForActivity(activity)
            }, {
                shape: "circle",
                text: "endpoint in a different activity",
                color: colors.endpoints.OUTSIDE_ACTIVITY
            }, {
                shape: "circle",
                text: "external endpoint",
                color: colors.endpoints.EXTERNAL
            }];

        })
        .factory("endpointTooltip", function(preferences) {
            return function(endpoint) {
                return [
                    "<div>",
                        "Endpoint",
                    "</div>",
                    "<div>",
                        endpoint.ip,
                    "</div>",
                    "<div>",
                        preferences.showDomainNames ? endpoint.host : "",
                    "</div>",
                    "<div>",
                        "Activity #" + endpoint.activity.index,
                    "</div>",
                    "<div>",
                        "Connections: ",
                    endpoint.totalConnections,
                    "</div>"

                ].join("");
            };
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
