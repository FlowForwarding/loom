'use strict';

angular.module("nci.services.nciConnection", [
    'nci.services.socket'
])
    .constant("connectionTimeout", 300000)
    .factory("connection", [
        "socket",
        "$q",
        "$timeout",
        "connectionTimeout",
        "$rootScope",
    function(socket, $q, $timeout, connectionTimeout, $rootScope) {
        var connection = {
                ne: 0,
                nep: 0,
                qps: 0,
                nci: 0,
                nciData: [],
                collectors: 0,
                startTime: null,
                close: function() {
                    this.socket.close();
                    this.nciData = [];
                    this.ne = 0;
                    this.nep = 0;
                    this.qps = 0;
                    this.nci = 0;
                    this.collectors = 0;
                    this.startTime = null;
                },
                getCollectors: function() {
                    return sendRequest(this.socket, {
                        action: "collectors",
                        Time: Date.now()
                    }, "collectors", function (data) {
                        return data.Collectors;
                    });
                },
                getDetails: function() {
                    return sendRequest(this.socket, {
                            action: "NCIDetails",
                            Time: Date.now()
                        }, "NCIDetails", function(data) {
                            return {
                                communities: data.Communities,
                                activities: data.CommunityGraph,
                                labels: data.Labels,
                                nci: data.NCI
                            };
                        }
                    );
                },
                getMore: function(startDate, endDate) {
                    startDate = connection.startTime < startDate ? startDate : connection.startTime;
                    endDate = endDate || Date.now();

                    return sendRequest(this.socket, {
                            request: "more_data",
                            start: convertDateForServer(startDate),
                            end: convertDateForServer(endDate),
                            max_items: "200",
                            Time: Date.now()
                        }, "more", function(data) {
                            data.forEach(function(data) {
                                connection.nciData.push([
                                    Date.parse(data.Time),
                                    data.NCI
                                ]);
                            });
                            connection.nciData = connection.nciData.sort(function(d1, d2) {
                                return d1[0] - d2[0];
                            });
                            return connection.nciData;
                        }
                    );
                }
            },
            handler = {
                'NCI': function(data) {
                    connection.nci = data.NCI;
                    connection.nciData.push([
                        Date.parse(data.Time),
                        data.NCI
                    ]);
                    // TODO: check if we really need sort here
                    connection.nciData = connection.nciData.sort(function(d1, d2) {
                        return d1[0] - d2[0];
                    });
                    $rootScope.$digest();
                },
//                'more': function(data) {
//                },
                'NEP': function(data) {
                    connection.ne = data.NE;
                    connection.nep = data.NEP;
                    $rootScope.$digest();
                },
                'QPS': function(data) {
                    connection.qps = data.QPS;
                    $rootScope.$digest();
                },
                'Collectors': function(data) {
                    connection.collectors = data.COLLECTORS;
                    $rootScope.$digest();
                }
            },
            
            url = "ws://" + location.host + "/clientsock.yaws";
            // url = "ws://localhost:28080/clientsock.yaws";
            // url = "ws://10.48.11.64:28080/clientsock.yaws";
            // url = "ws://10.48.2.81:28080/clientsock.yaws";


        function convertDateForServer (date){
            //we need to get such format in UTC 2013-10-27T13:11:39Z for server
            var returnDate = new Date(date).toISOString();
            returnDate = returnDate.substring(0, returnDate.length - 5) + "Z";
            return returnDate;
        }

        function handleMessage(event) {
            var data = JSON.parse(event.data),
                action = data.action || "more";

            if (action in handler) {
                handler[action](data);
            }
        }

        function rejectByTimeout(deferred) {
            $timeout(function() {
                deferred.reject("Connection timeout");
            }, connectionTimeout);
        }

        function sendRequest(socket, request, responseAction, dataParser) {
            var deferred = $q.defer();

            rejectByTimeout(deferred);

            handler[responseAction] = function(data) {
                deferred.resolve(dataParser(data));
            };
            socket.send(JSON.stringify(request));

            return deferred.promise;
        }

        function connect() {
            var deferred = $q.defer();

            if (!connect.socketPromise) {
                connect.socketPromise = socket(url)
                    .then(function(socket) {
                        // init nci connection
                        var handleHelloMessage = function(event) {
                            var data = JSON.parse(event.data);
                            if (data.action === "hello") {
                                connection.startTime = Date.parse(data.start_time);
                                deferred.resolve(socket);
                            }
                        };
                        rejectByTimeout(deferred);

                        socket.addEventListener("message", handleHelloMessage);
                        deferred.promise.finally(function() {
                            socket.removeEventListener("message", handleHelloMessage);
                        });
                        socket.send("START_DATA");

                        deferred.promise.catch(function() {
                            connect.socketPromise = null;
                            $rootScope.$broadcast("connectionClosed", "Server(" + url + ") is not Tapestry instance.");
                        });

                        socket.addEventListener("message", handleMessage);
                        socket.addEventListener("close", function() {
                            connect.socketPromise = null;
                            $rootScope.$broadcast("connectionClosed", "Connection refused by Server(" + url + ").");
                        });

                        return deferred.promise;
                    })
                    .then(function(socket) {
                        connection.socket = socket;
                        $rootScope.$broadcast("connected");
                        return connection;
                    });

                connect.socketPromise.catch(function() {
                    connect.socketPromise = null;
                    $rootScope.$broadcast("connectionClosed", "Unable to establish connection. Server(" + url + ") not responding");
                });

            }

            return connect.socketPromise;
        }

        connect.setUrl = function(newUrl) {
            connection.close();
            connect.socketPromise = null;
            url = newUrl;
        };
        connect.getUrl = function() {
            return url;
        };

        return connect;
    }]);