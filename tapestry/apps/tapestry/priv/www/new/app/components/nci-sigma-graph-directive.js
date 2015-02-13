(function (angular) {
    'use strict';

    angular.module('nci.components.nciSigmaGraph', [])
        .directive("nciSigmaGraph", function($log, $rootScope) {
            return {
                restrict: "E",
                scope: {
                    nodes: "=",
                    edges: "=",
                    nodeClick: "=nciGraphNodeClick"
                },
                template: '<div class="nci-graph-container"></div>',
                link: function($scope, el) {
                    var graph = {
                            nodes: $scope.nodes,
                            edges: $scope.edges
                        },
                        s = new sigma({
                            graph: graph,
                            renderer: {
                                type: sigma.renderers.canvas,
                                //type: sigma.renderers.webgl,
                                container: el.children().get(0)
                            },
                            batchEdgesDrawing: true
                            //container:
                        }),
                        forceConfig = {
                            barnesHutTheta: 0.5,
                            gravity: 1,
                            strongGravityMode: true,
                            //adjustSizes: true,
                            edgeWeightInfluence: 0.2,
                            slowDown: 100,
                            outboundAttractionDistribution: true
                        };

                    $scope.$watchCollection("nodes", function() {
                        s.killForceAtlas2();

                        var nodesMap = new Map(s.graph.nodes().map(function(n) {return [n.id, n];})),
                            edgesMap = new Map(s.graph.edges().map(function(n) {return [n.id, n];}));


                        var nodes = $scope.nodes.map(function(node) {
                            return nodesMap.get(node.id) || node;
                        });
                        nodesMap = new Map($scope.nodes.map(function(n) {return [n.id, n];}));

                        var edges = $scope.edges.map(function(edge) {
                            return edgesMap.get(edge.id) || edge;
                        });
                        edgesMap = new Map($scope.edges.map(function(n) {return [n.id, n];}));


                        s.graph.clear();
                        s.graph.read({
                            nodes: nodes,
                            edges: edges
                        });

                        $log.info("Endpoints On Screen:", nodes.length);
                        $log.info("Flows On Screen:", edges.length);
                        s.refresh();
                        startLayout();

                    });

                    function startLayout() {
                        if (isVisible()) {
                            s.startForceAtlas2(forceConfig);
                        }
                    }

                    function isVisible() {
                        return el.is(":visible");
                    }

                    $rootScope.$watch(isVisible, function(visible) {
                        if (!visible) {
                            s.killForceAtlas2();
                        } else {
                            s.startForceAtlas2(forceConfig);
                        }
                    });

                    CustomShapes.init(s);
                    s.bind('clickNode', function(event) {
                        if ($scope.nodeClick) {
                            $scope.nodeClick(event.data.node);
                        }
                    });

                    startLayout();

                    el.on("$destroy", function() {
                        s.killForceAtlas2();
                    });

                    s.settings({
                        defaultEdgeColor: "#DDD",
                        edgeColor: "default"
                        //batchEdgesDrawing: true
                    });

                    var dragListener = new sigma.plugins.dragNodes(s, s.renderers[0]);

                    dragListener.bind('startdrag', function(event) {
                        s.killForceAtlas2();
                    });
                    //dragListener.bind('drag', function(event) {
                    //    console.log(event);
                    //});
                    //dragListener.bind('drop', function(event) {
                    //    console.log(event);
                    //});
                    dragListener.bind('dragend', function(event) {
                        startLayout();
                    });

                }
            };
        });

})(angular);
