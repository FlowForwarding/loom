(function (angular) {
    'use strict';

    angular.module('nci.endpointsView.GraphController', [
        'nci.services.colorForActivity',
        'nci.components.nciSigmaGraph'
    ])
        .controller('EndpointsGraphController', [
            "$scope",
            "colorForActivity",
            "endpointTooltip",
            function($scope, colorForActivity, endpointTooltip) {
                $scope.edges = [];
                $scope.nodes = [];

                var rowsCount = 5;

                function createEndpointNode(endpoint) {
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

                function updateNodePosition(rowsCount, index, node) {
                    index = index || 0;
                    node.x = ((index%rowsCount) * 50) + (Math.random()*2 - 1)*20;
                    node.y = ((Math.floor(index/rowsCount)) * 50) + (Math.random()*2 - 1)*20;

                    return node;
                }

                var endpoints = $scope.endpoints,
                    details = endpoints.all(),
                    edgesSet = new Set();

                rowsCount = Math.round(Math.sqrt((new Set(details.map(function(ep) {return ep.activity;}))).size));

                $scope.edges = [];
                $scope.nodes = details.map(function(endpoint, index) {

                    Object.keys(endpoint.connections).forEach(function(targetIp) {
                        var targetEndpoint = endpoint.connections[targetIp],
                            id = endpoint.ip + "_" + targetEndpoint.ip;

                        if (!edgesSet.has(id)) {
                            $scope.edges.push({
                                id: id,
                                source: endpoint.ip,
                                target: targetEndpoint.ip,
                                weight: 0.2,
                                size: 0.1
                            });

                            edgesSet.add(id);
                            edgesSet.add(targetEndpoint.ip + "_" + endpoint.ip);
                        }
                    });
                    return updateNodePosition(rowsCount, endpoint.activity.index, createEndpointNode(endpoint));
                });

                // this is fake node, to draw graph nodes smaller
                var anchor = {
                    id: "anchor",
                    size: 5,
                    weight: 0,
                    x: 0,
                    y: 0,
                    color: "transparent"
                };
                $scope.nodes.push(anchor);

                $scope.tooltip = function(node) {
                    return endpointTooltip(node.endpoint);
                };

            }]);

})(angular);
