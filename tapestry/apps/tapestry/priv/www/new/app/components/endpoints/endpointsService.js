(function (angular) {
    'use strict';

    angular.module('nci.components.services.endpointsService', [
        'ngMaterial',
        'nci.services.export',
        'nci.components.nciSigmaGraph'
    ])
        .controller("endpointsDialogController", function($scope, $mdDialog, activity, exportToCSV, endpointTooltip, colors) {
            var defaultEndpointColor = colors.endpoints.default,
                edgesSet = new Set(),
                edges = [],
                nodes = [],
                endpointsSet = new Set();

            function createEdge(target, source, weight, size, isOutside, isExternal) {
                var id = target + "_" + source;
                isOutside = isOutside || false;

                if (!edgesSet.has(id) && endpointsSet.has(target) && endpointsSet.has(source)) {
                    var edge = {
                        id: id,
                        color: "#999",
                        source: source,
                        target: target,
                        weight: weight,
                        isOutside: isOutside,
                        external: isExternal,
                        size: size
                    };
                    edges.push(edge);
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
                    color = isOutside(endpoint) ? colors.endpoints.OUTSIDE_ACTIVITY : defaultEndpointColor;


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
                $scope.nodes = showOutside ? nodes.slice() : nodes.slice().filter(function(n) {return !n.isOutside});
                $scope.edges = showOutside ? edges.slice() : edges.slice().filter(function(n) {return !n.isOutside});
            }

            function updateShowInternalDisplay() {
                var showInternalOnly = $scope.showInternalOnly;
                $scope.nodes = !showInternalOnly ? $scope.nodes : $scope.nodes.filter(function(n) {return !n.external});
                $scope.edges = !showInternalOnly ? $scope.edges : $scope.edges.filter(function(n) {return !n.external});
            }

            function updateExternalDisplay() {
                $scope.nodes = nodes.map(updateNodeColor);
            }

            $scope.showOutside = false;
            $scope.showExternal = false;
            $scope.showInternalOnly = false;

            $scope.updateDisplay = function() {
                removeAnchor();
                updateExternalDisplay();
                updateOutsideDisplay();
                updateShowInternalDisplay();
                addAnchor();

                updateGraph();
            };


            function createEndpointNode(endpoint) {
                endpointsSet.add(endpoint.ip);
                var outside = isOutside(endpoint);

                endpoint.getConnections().forEach(function(ep) {
                    createEdge(endpoint.ip, ep.ip, 0.2, 0.1, isOutside(ep) || outside, endpoint.external || ep.external);
                });

                return {
                    id: endpoint.ip,
                    endpoint: endpoint,
                    external: endpoint.external,
                    isOutside: outside,
                    size: 1,
                    label: endpoint.ip,
                    x: Math.random()*20 - 10,
                    y: Math.random()*20 - 10,
                    color: defaultEndpointColor
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
                $scope.nodes.pop();
                //insideNodes.pop();
            }

            function addAnchor() {
                $scope.nodes.push(anchor);
                //insideNodes.push(anchor);
            }

            updateOutsideDisplay();
            addAnchor();

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
                color: defaultEndpointColor
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
