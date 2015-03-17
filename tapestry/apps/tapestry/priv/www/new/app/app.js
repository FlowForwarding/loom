'use strict';

angular.module('nci', [
        'ui.router',
        'ngMaterial',
        'nci.views',
        'nci.monitor',
        'nci.services.nciConnection',
        'nci.services.nciEndpointModel',
        'sigmaGraphOptions'
    ])
    .config(['$mdThemingProvider', function($mdThemingProvider) {}])
    .config(function($stateProvider, $urlRouterProvider) {
        $stateProvider
            .state("monitor", {
                url: "/monitor",
                templateUrl: "views/monitor/MonitorViewTemplate.html",
                controller: 'MonitorViewController',
                data: {
                    name: "NCI Monitor"
                },
                resolve: {
                    con: ["connection", function(connection) {
                        return connection();
                    }]
                }

            })
            .state("collectors", {
                url: "/collectors",
                templateUrl: "views/collectors/CollectorsViewTemplate.html",
                data: {
                    name: "Collectors"
                },
                resolve: {
                    collectors: function($q, $timeout, connection) {
                        return connection().then(function(con) {
                            return con.getCollectors();
                        });
                    }
                },
                controller: 'CollectorsViewController'
            })
            .state("details", {
                url: "/details",
                abstract: true
            })


            .state("details.activities", {
                url: "/activities",
                abstract: true,
                data: {
                    name: "Activities"
                },

                views: {
                    "toolbar@": {
                        templateUrl: "views/details/TabViewTemplate.html",
                        controller: "TabViewController"
                    },
                    "@": {
                        template: '<ui-view flex layout="column"></ui-view>'
                    }
                },
                resolve: {
                    activities: function(activitiesPromise) {
                        return activitiesPromise;
                    }
                }
            })
            .state("details.activities.histogram", {
                url: "/histogram",
                data: {
                    index: 0
                },
                views: {
                    "": {
                        templateUrl: "views/details/activities/HistogramViewTemplate.html",
                        controller: "ActivitiesHistogramController"
                    }
                }
            })
            .state("details.activities.graph", {
                url: "/graph",
                data: {
                    index: 1
                },
                views: {
                    "": {
                        templateUrl: "views/details/activities/GraphViewTemplate.html",
                        controller: "ActivitiesGraphController"
                    }
                }
            })
            .state("details.activities.table", {
                url: "/table",
                data: {
                    index: 2
                },
                views: {
                    "": {
                        templateUrl: "views/details/activities/TableViewTemplate.html",
                        controller: "ActivitiesTableController"
                    }
                }
            })


            .state("details.endpoints", {
                url: "/endpoints/{endpoint}",
                abstract: true,
                data: {
                    name: "All Endpoints"
                },
                views: {
                    "toolbar@": {
                        //template: '<div class="md-toolbar-tools">Test</div><ng-include src="\'./views/details/TabViewTemplate.html\'"></ng-include>',
                        templateUrl: "views/details/TabViewTemplate.html",
                        controller: "TabViewController"
                    },
                    "@": {
                        template: '<ui-view flex layout="column"></ui-view>'
                    }
                },
                resolve: {
                    endpoints: function(endpointsPromise, $stateParams) {
                        var ip = $stateParams.endpoint;

                        return endpointsPromise.then(function(ep) {
                            if (ip) {
                                var endpoint = ep.byIp(ip),
                                    connections = endpoint.getConnections();
                                //connections.push(endpoint);
                                return connections;
                            }
                            return ep.all();
                        });
                    }
                }
            })
            .state("details.endpoints.histogram", {
                url: "/histogram",
                data: {
                    index: 0
                },
                views: {
                    "": {
                        templateUrl: "views/details/endpoints/HistogramViewTemplate.html",
                        controller: "EndpointsHistogramController"
                    }
                }
            })
            .state("details.endpoints.graph", {
                url: "/graph",
                data: {
                    index: 1
                },
                resolve: {
                    endpoints: function(endpoints, $stateParams, endpointsPromise) {
                        var ip = $stateParams.endpoint;
                        return endpointsPromise.then(function(ep) {
                            if (ip) {
                                var endpoint = ep.byIp(ip);
                                // Make a copy of endpoints array
                                endpoints = endpoints.slice();
                                endpoints.push(endpoint);
                            }
                            return endpoints;
                        });
                    }
                },
                views: {
                    "": {
                        templateUrl: "views/details/endpoints/GraphViewTemplate.html",
                        controller: "EndpointsGraphController"
                    }
                }
            })
            .state("details.endpoints.table", {
                url: "/table",
                data: {
                    index: 2
                },
                views: {
                    "": {
                        templateUrl: "views/details/endpoints/TableViewTemplate.html",
                        controller: "EndpointsTableController"
                    }
                }
            });

        $urlRouterProvider
            .when("/details/endpoints", "/details/endpoints//histogram")
            .when("/details/activities", "/details/activities/histogram")
            .otherwise("/monitor");
    })
    .controller("connectionController", [
        "$mdToast",
        "$scope",
        "$timeout",
        function($mdToast, $scope, $timeout) {
            var toast = null;
            $scope.$on("connectionClosed", function(event, message) {
                $mdToast.hide();
                $timeout(function() {
                    $mdToast
                        .show({
                            template: [
                                '<md-toast>',
                                    '<span flex>', message, '</span>',
                                    '<md-button class="md-primary"  ng-click="retry()">',
                                        'RETRY',
                                    '</md-button>',
                                    '<md-button ng-click="changeServer()">',
                                        'CHANGE SERVER',
                                    '</md-button>',
                                '</md-toast>'
                            ].join(''),
                            position: "top right",
                            controller: "connectionToastController",
                            hideDelay: 0
                        });
                }, 1);
            });
            $scope.$on("connected", function() {
                $mdToast.hide();
            });

        }
    ])
    .controller("connectionToastController", [
        "connection",
        "$scope",
        "$mdToast",
        "$state",
        function(connection, $scope, $mdToast, $state) {
            $scope.retry = function() {
                $mdToast.hide();
                connection()
                    .then(function() {
                        $state.go($state.current, {}, {reload: true});
                    });
            };
            $scope.changeServer = function() {
                $mdToast.hide();
            };
        }
    ])
    .controller("main", [
        "$mdDialog",
        "$q",
        "$scope",
        "$mdSidenav",
        "$state",
        "$stateParams",
        function($mdDialog, $q, $scope, $mdSidenav, $state, $stateParams) {
            var alert = null,
                showDefer = null;

            function hideDialog() {
                showDefer.promise.then(function() {
                    $mdDialog.hide(alert);
                    showDefer = null;
                });
            }

            $scope.$on("$routeChangeStart", function() {
                showDefer = $q.defer();
                alert = $mdDialog
                    .show({
                        template: ['<md-dialog no-background-dialog>',
                                '<md-content layout="row">',
                                    '<span flex></span>',
                                    '<md-progress-circular md-mode="indeterminate"></md-progress-circular>',
                                    '<span flex></span>',
                                '</md-content>',
                            '</md-dialog>'].join(""),
                        clickOutsideToClose: false,
                        escapeToClose: false,
                        onComplete: function() {
                            showDefer.resolve();
                        }
                    })
                    .finally(function() {
                        alert = null;
                    });
            });
            $scope.$on("$routeChangeSuccess", hideDialog);
            $scope.$on("$routeChangeError", function() {
                hideDialog();
                console.log("Issue");
            });

            $scope.toggleSideNav = function() {
                $mdSidenav('left').toggle();
            };

            function getViewName() {
                return $stateParams.endpoint ? $stateParams.endpoint : $state.current.data.name;
            }

            $scope.allEndpoints = function() {
                $state.go($state.current, {endpoint: null}, {reload: true});
            };

            $scope.$on("$stateChangeSuccess", function(event, current) {
                $scope.viewName = getViewName();
                $scope.showAllEndpoints = !!$stateParams.endpoint;
            });

            $scope.search = function(event) {
                $mdDialog.show({
                    templateUrl: "./views/search/SearchViewTemplate.html",
                    clickOutsideToClose: true,
                    escapeToClose: true,
                    targetEvent: event,
                    onComplete: function() {
                        console.log("done!!");
                    },
                    controller: function($scope, modelPromise) {
                        $scope.search = function(query) {
                            modelPromise.then(function(model) {
                                $scope.results = model.findEndpoint(query);
                            });
                        };
                    }
                });
            };

        }
    ])
    .controller("optionsController", [
        "$scope",
        "$mdBottomSheet",
        function($scope, $mdBottomSheet, nciConnection) {
            $scope.options = function() {
                $mdBottomSheet.show({
                    template: ['<md-bottom-sheet>',
                            '<md-button ng-click="configureLayout()">Graph Force layout config</md-button>',
                            '<md-switch ng-model="showDomainNames" ng-change="updatePreferences()" aria-label="Finished?">',
                            'Show domain names',
                            '</md-switch>',
                            '<md-divider></md-divider>',
                            '<form ng-submit="reconnect()" layout="row" layout-align="center center">',
                                '<md-input-container flex>',
                                    '<label>Tapestry server URL</label>',
                                    '<input type="text" ng-submit="reconnect()" ng-model="serverUrl" required md-maxlength="50">',
                                '</md-input-container>',
                                '<md-button >Connect</md-button>',
                            '</form>',
                        '</md-bottom-sheet>'].join(""),
                    controller: "optionsSheetController"
                });
            };
        }
    ])
    .value("preferences", {
        showDomainNames: false
    })
    .controller("optionsSheetController", [
        '$scope',
        '$rootScope',
        "connection",
        "$state",
        "preferences",
        "$mdBottomSheet",
        function($scope, $rootScope, connection, $state, preferences, $mdBottomSheet) {
            $scope.serverUrl = connection.getUrl();
            $scope.reconnect = function() {
                connection.setUrl($scope.serverUrl);
                connection().then(function() {
                    $state.go($state.current, {}, {reload: true});
                });
            };

            console.log(preferences, preferences.showDomainNames, $scope.showDomainNames);
            $scope.showDomainNames = preferences.showDomainNames;

            $scope.updatePreferences = function() {
                preferences.showDomainNames = $scope.showDomainNames;
                console.log(preferences, $scope.showDomainNames);
                $rootScope.$broadcast("app:preferencesChanged", preferences);
            };

            $scope.configureLayout = function() {
                $mdBottomSheet.hide().then(function() {
                    $mdBottomSheet.show({
                        template:
                        '<md-bottom-sheet>' +
                            '<ng-include src="\'./components/sigma-graph/nci-sigma-graph-options.html\'"></ng-include>' +
                        '</md-bottom-sheet>',
                        controller: "ForceLayoutConfigController"
                    });
                });
            };
        }
    ])
    .controller("tapestryNavigation", function($scope, $state) {
        $scope.showMonitor = function() {
            $state.go("monitor");
        };
        $scope.showCollectors = function() {
            $state.go("collectors");
        };
        $scope.showDetails = function() {
            $state.go("details.endpoints.histogram");
        };
    });
