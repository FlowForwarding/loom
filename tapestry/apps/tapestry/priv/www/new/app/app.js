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
    .value('googleChartApiConfig', {
        version: '1.1',
        optionalSettings: {
            packages: ['line', 'bar'],
            language: 'en'
        }
    })
    .config(['$mdThemingProvider', function($mdThemingProvider) {}])
    .config(function($stateProvider, $urlRouterProvider) {
        $stateProvider
            .state("monitor", {
                url: "/monitor",
                templateUrl: "views/monitor/MonitorViewTemplate.html",
                controller: 'MonitorViewController',
                resolve: {
                    con: ["connection", function(connection) {
                        return connection();
                    }]
                }

            })
            .state("collectors", {
                url: "/collectors",
                templateUrl: "views/collectors/CollectorsViewTemplate.html",
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
                url: "/endpoints",
                abstract: true,
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
                    endpoints: function(endpointsPromise) {
                        return endpointsPromise;
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
            .when("/details/endpoints", "/details/endpoints/histogram")
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
        "$route",
        function(connection, $scope, $mdToast, $route) {
            $scope.retry = function() {
                $mdToast.hide();
                connection()
                    .then($route.reload);
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
        function($mdDialog, $q, $scope, $mdSidenav) {
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
        "$route",
        "preferences",
        "$mdBottomSheet",
        function($scope, $rootScope, connection, $route, preferences, $mdBottomSheet) {
            $scope.serverUrl = connection.getUrl();
            $scope.reconnect = function() {
                connection.setUrl($scope.serverUrl);
                connection().then(function() {
                    $route.reload();
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
    .controller("tapestryNavigation", ["$scope", "$location", function($scope, $location) {
        $scope.showMonitor = function() {
            $location.path("/monitor");
        };
        $scope.showCollectors = function() {
            $location.path("/collectors");
        };
        $scope.showDetails = function() {
            $location.path("/details/activities");
        };
    }]);
