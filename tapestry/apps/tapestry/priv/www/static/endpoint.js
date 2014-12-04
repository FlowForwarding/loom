(function() {
    var activitiesMap = {},
        endpointsMap = {},
        hostsMap = {};


    function createActivity(activity) {
        var index = activity.NameIndex,
            mainIP = activity.Label,
            endpoints = activity.Endpoints,
            interactions = activity.Interactions,
            activity = new Activity(index, mainIP, endpoints, interactions);

        activitiesMap[mainIP] = activity;
        return activity;
    }

    function Activity(index, mainEndpointIP, endpoints, interactions) {
        this.index = index;
        this.size = 0;
        this.mainEndpoint = getOrCreateEndpoint(mainEndpointIP);
        this.endpoints = {};

        endpoints.forEach(this.addEndpoint.bind(this));

        interactions.forEach(function(interaction) {
            var ep1 = getOrCreateEndpoint(interaction[0]),
                ep2 = getOrCreateEndpoint(interaction[1]);

            ep1.addConnection(ep2);
            ep2.addConnection(ep1);
        });
    }

    Activity.prototype.addEndpoint = function(ip) {
        var endpoint = getOrCreateEndpoint(ip);
        this.endpoints[ip] = endpoint;
        setActivityForEndpoint(this, endpoint);
        this.size += 1;
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
        this.external = NCI.isExternal(ip);
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

    var endpointsList = [],
        activitiesList = [];


    NCI.model = {
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
    }

})();
