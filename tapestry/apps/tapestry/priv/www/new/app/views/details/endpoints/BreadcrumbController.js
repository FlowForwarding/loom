(function (angular) {
    'use strict';

    angular.module('nci.views.details.endpoints.BreadcrumbController', [])
        .controller("BreadcrumbController", function($scope, $state, endpointsBreadcrumbs) {

            $scope.breadcrumbs = endpointsBreadcrumbs.all();

            $scope.goTo = function(ip) {
                if (ip) {
                    $state.go($state.current, {endpoint: ip});
                } else {
                    $state.go($state.current, {endpoint: ""});
                }
            };
        })
        .factory("endpointsBreadcrumbs", function($rootScope, $stateParams) {
            var breadcrumbs = [{
                text: "ALL ENDPOINTS",
                ip: ""
            }];

            if ($stateParams.endpoint) {
                addEndpointToBreadcrumb($stateParams.endpoint);
            }

            function addEndpointToBreadcrumb(endpoint) {
                breadcrumbs.push({
                    text: endpoint,
                    ip: endpoint
                });
            }

            function updateBreadcrumbs(newEndpoint) {
                var indexOfBreadcrumb = -1;

                breadcrumbs.forEach(function(breadcrumb, index) {
                    if (breadcrumb.ip == newEndpoint) {
                        indexOfBreadcrumb = index;
                        return false;
                    }
                });

                if (indexOfBreadcrumb < 0) {
                    addEndpointToBreadcrumb(newEndpoint);
                } else {
                    breadcrumbs = breadcrumbs.slice(0, indexOfBreadcrumb + 1);
                }

            }

            $rootScope.$on("$stateChangeStart", function(event, toState, toParams, fromState, fromParams) {
                if (toState.name.indexOf("details.endpoints") > -1
                        && fromParams.endpoint != toParams.endpoint) {
                    console.log(toParams);
                    updateBreadcrumbs(toParams.endpoint);
                }
            });

            return {
                all: function() {
                    return breadcrumbs;
                }
            };
        });

})(angular);
