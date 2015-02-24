(function (angular) {
    'use strict';

    angular.module('nci.components.nciSigmaGraph', [])
        .directive("nciSigmaGraph", function($log, $rootScope) {
            return {
                restrict: "E",
                scope: {
                    nodes: "=",
                    edges: "=",
                    config: "=",
                    nodeClick: "=nciGraphNodeClick",
                    tooltip: "=?"
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
                        graphConfig = {
                            defaultEdgeColor: "#DDD",
                            edgeColor: "default",
                            enableHovering: false,
                            drawLabels: false
                            //batchEdgesDrawing: true
                        },
                        forceConfig = {
                            barnesHutTheta: 0.5,
                            gravity: 1,
                            strongGravityMode: true,
                            //adjustSizes: true,
                            edgeWeightInfluence: 0.2,
                            slowDown: 100,
                            outboundAttractionDistribution: true
                        };

                    // dirty hack to delegate graph redraw
                    if ($scope.config) {
                        $scope.$watch(function() {
                            return $scope.config.redraw;
                        }, updateGraph);
                    }

                    function updateGraph() {
                        s.killForceAtlas2();

                        var nodesMap = new Map(s.graph.nodes().map(function(n) {return [n.id, n];})),
                            edgesMap = new Map(s.graph.edges().map(function(n) {return [n.id, n];}));


                        var nodes = $scope.nodes.map(function(node) {
                            var newNode = nodesMap.get(node.id) || node;
                            newNode.color = node.color;
                            return newNode;
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
                    }

                    $scope.$watchCollection("nodes", updateGraph);

                    function startLayout() {
                        if (isVisible()) {
                            s.settings(graphConfig);
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
                            // since sigma clones nodes internally, need to find our own node
                            $scope.nodeClick($scope.nodes.filter(function(node) {
                                return node.id == event.data.node.id;
                            })[0]);
                        }
                    });

                    var $tooltip = $('<md-card style="position: fixed;background:white;z-index:81"><md-card-content>tooltip</md-card-content></md-card>');
                    $("body").append($tooltip);

                    $scope.tooltip = $scope.tooltip || function(node) {
                        return node.id;
                    };

                    function fixY(y) {
                        var vHeight = $(window).height(),
                            tHeight = $tooltip.height();
                        return (y + tHeight) > vHeight ? vHeight - tHeight : y;
                    }
                    function fixX(x) {
                        var vWidth = $(window).width(),
                            tWidth = $tooltip.width();
                        return (x + tWidth) > vWidth ? vWidth - tWidth : x;
                    }

                    s.bind("overNode", function(event) {
                        var x = fixX(event.data.captor.clientX + 10),
                            y = fixY(event.data.captor.clientY + 10);


                        $tooltip.children().html($scope.tooltip(event.data.node));
                        $tooltip.offset({
                            top: y,
                            left: x
                        });

                        //console.log(arguments);
                    });

                    s.bind("outNode", function() {
                        if (!$tooltip.is(":hover")) {
                            $tooltip.offset({
                                top: -2000,
                                left: -2000
                            });
                        }
                    });

                    startLayout();

                    el.on("$destroy", function() {
                        s.killForceAtlas2();
                    });

                    s.settings(graphConfig);

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
