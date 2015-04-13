(function (angular) {
    'use strict';

    angular.module('nci.views', [
        "nci.views.monitor.MonitorViewController",
        "nci.views.collectors.CollectorsViewController",

        "nci.views.details.TabViewController",

        "nci.views.details.endpoints.BreadcrumbController",

        "nci.views.details.endpoints.EndpointsGraphController",
        "nci.views.details.endpoints.EndpointsHistogramController",
        "nci.views.details.endpoints.EndpointsTableController",

        "nci.views.details.activities.ActivitiesHistogramController",
        "nci.views.details.activities.ActivitiesGraphController",
        "nci.views.details.activities.ActivitiesTableController"
    ]);

})(angular);
