(function(angular) {
    'use strict';

    angular.module("nci.services.model", [
        "nci.services.nciConnection",
        "nci.services.nciIsExternal"
    ])
        .factory("modelPromise", ["connection", "checkExternal", function(connection, isExternal) {
            var activitiesMap,
                endpointsMap,
                hostsMap,
                endpointsList,
                activitiesList;


            function createActivity(activity) {
                var mainIP = activity.Label,
                    endpoints = activity.Endpoints,
                    interactions = activity.Interactions;

                activity = new Activity(mainIP, endpoints, interactions);

                activitiesMap[mainIP] = activity;
                return activity;
            }

            function Activity(mainEndpointIP, endpoints, interactions) {
                this.index = 0;
                this.size = 0;
                this.mainEndpoint = getOrCreateEndpoint(mainEndpointIP);
                this.endpoints = {};
                this.activities = {};
                this.internalFlows = 0;
                this.externalFlows = 0;

                endpoints.forEach(this.addEndpoint, this);

                interactions.forEach(function(interaction) {
                    var ep1 = getOrCreateEndpoint(interaction[0]),
                        ep2 = getOrCreateEndpoint(interaction[1]);

                    if (!ep1.external && !ep2.external) {
                        this.internalFlows += 1;
                    } else {
                        this.externalFlows += 1;
                    }

                    ep1.addConnection(ep2);
                    ep2.addConnection(ep1);
                }, this);

            }

            Object.defineProperty(Activity.prototype, 'avgInternalFlows', {
                get: function() {
                    return this.internalFlows / this.size;
                }
            });

            Activity.prototype.addEndpoint = function(ip) {
                var endpoint = getOrCreateEndpoint(ip);
                this.endpoints[ip] = endpoint;
                setActivityForEndpoint(this, endpoint);
                this.size += 1;
            };

            Activity.prototype.getEndpoints = function() {
                var endpoints = this.endpoints;
                return Object.keys(endpoints).map(function(key) {return endpoints[key];});
            };

            Activity.prototype.hasEndpoint = function(endpoint) {
                return endpoint.ip in this.endpoints;
            };

            function isInOneActivity(ep1, ep2) {
                return ep1.activity === ep2.activity;
            }

            function getOrCreateEndpoint(ip) {
                var endpoint = endpointsMap[ip] || new Endpoint(ip);
                endpointsMap[ip] = endpoint;
                return endpoint;
            }

            function setActivityForEndpoint(activity, endpoint) {
                endpoint.activity = activity;
            }

            function Endpoint(ip, activity) {
                this.ip = ip;
                this.external = isExternal(ip);
                this.connections = {};
                this.totalConnections = 0;
                this.externalConnections = 0;
                this.internalConnections = 0;
                this.outsideConnections = 0;
                this.activity = activity;
            }

            Object.defineProperty(Endpoint.prototype, 'host', {
                get: function() {
                    return hostsMap[this.ip];
                }
            });

            Endpoint.prototype.addConnection = function(endpoint) {
                if (!(endpoint.ip in this.connections)) {
                    this.connections[endpoint.ip] = endpoint;
                    this.totalConnections += 1;
                    if (endpoint.external) {
                        this.externalConnections += 1;
                    } else {
                        this.internalConnections += 1;
                    }

                    if (isInOneActivity(this, endpoint)) {
                        this.outsideConnections += 1;
                    }
                }
            };

            Endpoint.prototype.getConnections = function() {
                var connections = this.connections;
                return Object.keys(connections).map(function(key) {return connections[key]});
            };

            function processDetailsResult(details) {
                var sizesMap = {};

                activitiesMap = {};
                endpointsMap = {};
                hostsMap = {};

                function parseActivitySize(activity) {
                    var tmp = activity.split("|");
                    return {size: tmp.pop(), ip: tmp.pop()};
                }

                function updateSize(activity) {
                    activity.size = sizesMap[activity.mainEndpoint.ip];
                    return activity;
                }

                function addActivity(which, to) {
                    activitiesMap[to].activities[which] = activitiesMap[which];
                }

                details.activities.Endpoints.forEach(function(activityWithSize) {
                    var act = parseActivitySize(activityWithSize);
                    sizesMap[act.ip] = parseInt(act.size, 10);
                });

                Object.keys(details.labels).forEach(function(ip) {
                    hostsMap[ip] = details.labels[ip].Name;
                });


                activitiesList = details.communities
                    .map(createActivity)
                    .map(updateSize)
                    .sort(function(a1, a2) {
                        return a2.size - a1.size;
                    }).map(function(activity, index) {
                        activity.index = index + 1;
                        return activity;
                    });

                details.activities.Interactions.forEach(function(interaction) {
                    var activity1 = parseActivitySize(interaction[0]).ip,
                        activity2 = parseActivitySize(interaction[1]).ip;

                    addActivity(activity1, activity2);
                    addActivity(activity2, activity1);
                });

                endpointsList = Object.keys(endpointsMap).map(function(key) {return endpointsMap[key];});


                return {
                    getActivityByMainEndpoint: function(endpoint) {
                        return activitiesMap[endpoint.ip];
                    },
                    getEndpointByIp: function(ip) {
                        return endpointsMap[ip];
                    },
                    hostNameForIp: function(ip) {
                        return this.getEndpointByIp(ip).host;
                    },
                    endpoints: function() {
                        return endpointsList;
                    },
                    activities: function() {
                        return activitiesList;
                    }
                };

            }

            return connection().
                then(function(nci) {
                    return nci.getDetails();
                })
                .then(processDetailsResult);
        }]);

})(angular);
