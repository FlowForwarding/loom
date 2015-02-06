(function (angular) {
    'use strict';

    angular.module('nci.detailsView.GraphController', [
        'nci.services.colorForActivity',
        'nci.components.nciSigmaGraph'
    ])
        .controller('GraphController', ["$scope", "activitiesPromise", "colorForActivity", function($scope, activitiesPromise, colorForActivity) {
            $scope.edges = [];
            $scope.nodes = [];

            activitiesPromise.then(function(activities) {
                var details = activities.all(),
                    edgesSet = new Set(),
                    nodesSet,
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
                                $scope.nodes.push({
                                    id: endpoint.ip,
                                    size: 1,
                                    label: endpoint.ip,
                                    x: ((activity.index%rowsCount) * 50) + (Math.random()*2 - 1)*20, //,
                                    y: ((Math.floor(activity.index/rowsCount)) * 50) + (Math.random()*2 - 1)*20, //activity.index * 100,
                                    color: colorForActivity(endpoint.activity)
                                });
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

                        node.color = '#666';
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

                    return {
                        id: activity.mainEndpoint.ip,
                        expanded: false,
                        activity: true,
                        size: Math.log(activity.size),
                        label: "Activity #" + activity.index + "\n" + activity.mainEndpoint.ip,
                        x: Math.random()*20 - 10,
                        y: Math.random()*20 - 10,
                        color: '#666',
                        type: 'square'
                    };
                });

                nodesSet = new Set($scope.nodes.map(function(n) {return n.id;}));

            });
        }]);

})(angular);
