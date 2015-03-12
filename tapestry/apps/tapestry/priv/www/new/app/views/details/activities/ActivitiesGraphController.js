(function (angular) {
    'use strict';

    angular.module('nci.views.details.activities.ActivitiesGraphController', [
        'nci.services.colorForActivity',
        'nci.components.nciSigmaGraph'
    ])
        .controller('ActivitiesGraphController', [
            "$scope",
            "colorForActivity",
            "colors",
            "endpointTooltip",
            "$interval",
            "activities",
        function($scope, colorForActivity, colors, endpointTooltip, $interval, activities) {
            $scope.edges = [];
            $scope.nodes = [];

            var rowsCount = 5;

            function createActivityNode(activity) {
                return {
                    id: activity.mainEndpoint.ip,
                    expanded: false,
                    activity: activity,
                    size: Math.log(activity.size),
                    label: "Activity #" + activity.index + "\n" + activity.mainEndpoint.ip,
                    x: Math.random()*20 - 10,
                    y: Math.random()*20 - 10,
                    color: colors.activities.default,
                    type: 'square'
                };
            }

            function createEndpointNode(endpoint) {
                return {
                    id: endpoint.ip,
                    size: 1,
                    endpoint: endpoint,
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

            var details = activities.all(),
                edgesSet = new Set(),
                nodesSet;

            rowsCount = Math.round(Math.sqrt(details.length));

            $scope.expandNode = function(node) {
                var activity = activities.byIp(node.id);

                if (!node.activity) {return;}
                if (!node.expanded) {
                    node.color = "#AAA";
                    node.expanded = true;
                    Object.keys(activity.endpoints).forEach(function(ip) {
                        var endpoint = activity.endpoints[ip];
                        if (!nodesSet.has(ip)) {
                            $scope.nodes.push(updateNodePosition(rowsCount, activity.index, createEndpointNode(endpoint)));
                            nodesSet.add(ip);
                        }

                        Object.keys(endpoint.connections).forEach(function(ip) {
                            var id = endpoint.ip + "_" + ip;

                            if (!edgesSet.has(id) && nodesSet.has(ip)) {
                                edgesSet.add(id);
                                edgesSet.add(ip + "_" + endpoint.ip);

                                $scope.edges.push({
                                    id: id,
                                    source: endpoint.ip,
                                    target: ip,
                                    weight: 0.2,
                                    size: 0.1
                                });
                            }
                        });
                    });
                } else {
                    var nodesToRemove = new Set(Object.keys(activity.endpoints));

                    nodesToRemove.delete(node.id);

                    node.color = colors.activities.default;
                    node.expanded = false;

                    $scope.nodes = $scope.nodes.filter(function(node) {
                        if (node.activity) {
                            nodesToRemove.delete(node.id);
                            return true;
                        }
                        if (nodesToRemove.has(node.id)) {
                            nodesSet.delete(node.id);
                            return false;
                        }
                        return true;
                    });

                    $scope.edges = $scope.edges.filter(function(edge) {
                        if (nodesToRemove.has(edge.source) || nodesToRemove.has(edge.target)) {
                            edgesSet.delete(edge.source + "_" + edge.target);
                            edgesSet.delete(edge.target + "_" + edge.source);
                            return false;
                        }
                        return true;
                    });

                }
                $scope.$apply();
            };

            $scope.edges = [];

            $scope.nodes = details.map(function(activity) {

                Object.keys(activity.activities).forEach(function(targetIp) {
                    var targetActivity = activity.activities[targetIp],
                        id = activity.mainEndpoint.ip + "_" + targetActivity.mainEndpoint.ip;

                    if (!edgesSet.has(id)) {
                        $scope.edges.push({
                            id: id,
                            source: activity.mainEndpoint.ip,
                            target: targetActivity.mainEndpoint.ip,
                            weight: 0.1,
                            size: 0.5
                        });

                        edgesSet.add(id);
                        edgesSet.add(targetActivity.mainEndpoint.ip + "_" + activity.mainEndpoint.ip);
                    }
                });

                return createActivityNode(activity);
            });

            nodesSet = new Set($scope.nodes.map(function(n) {return n.id;}));

            $scope.tooltip = function(node) {
                if (node.activity) {
                    var activity = node.activity;
                    return [
                        "<div>",
                            "Activity #", activity.index,
                        "</div>",
                        "<div>",
                            activity.mainEndpoint.ip,
                        "</div>",
                        "<div>",
                            "Size: ",
                            activity.size,
                        "</div>",
                        "<div>",
                            "Connections: ",
                            Object.keys(activity.activities).length - 1,
                        "</div>"

                    ].join("");
                } else {
                    return endpointTooltip(node.endpoint);
                }
            };

            var endpointItem = {
                shape: "circle",
                text: "endpoint",
                color: colorForActivity({index: 0})
            };

            $interval(function() {
                endpointItem.color = colorForActivity({index: Math.floor(Math.random()*10)});
            }, 3000);

            $scope.legendKeys = [{
                shape: "square",
                text: "activity",
                color: colors.activities.default
            }, endpointItem, {
                shape: "circle",
                text: "external endpoint",
                color: colors.endpoints.EXTERNAL
            }];

        }]);

})(angular);
