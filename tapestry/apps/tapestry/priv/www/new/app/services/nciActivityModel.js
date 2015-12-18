(function(angular) {
    'use strict';

    angular.module("nci.services.nciActivityModel", [
        "nci.services.model"
    ])
        .factory("activitiesPromise", ["modelPromise", function(modelPromise) {
            return modelPromise
                .then(function(model) {
                    return {
                        //byIndex: function (index) {
                        //    return model.activities()[index - 1];
                        //},
                        byIp: function (ip) {
                            return model.getActivityByMainEndpoint(model.getEndpointByIp(ip));
                        },
                        filter: function(filterFn) {
                            return model.activities().filter(filterFn);
                        },
                        all: function () {
                            return model.activities();
                        }
                    };
                });
        }]);
})(angular);



/*
 createActivity: createActivity,
 getActivityByMainEndpoint: function(endpoint) {
     return activitiesMap[endpoint];
 },
 getEndpointByIp: function(ip) {
    return endpointsMap[ip];
 },
 parseLabels: function(labels) {
     Object.keys(labels).forEach(function(ip) {
         hostsMap[ip] = labels[ip].Name;
     });
 },
 hostNameForIp: function(ip) {
    return this.getEndpointByIp(ip).host;
 },
 parseActivities: function(activities) {
     activitiesList = activities.map(createActivity);
     endpointsList = Object.keys(endpointsMap).map(function(key) {return endpointsMap[key]});

     return activitiesList;
 },
 endpoints: function() {
     return endpointsList;
 },
 activities: function() {
    return activitiesList;
 }

 */



