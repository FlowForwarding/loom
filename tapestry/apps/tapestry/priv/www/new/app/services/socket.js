(function(angular) {
    'use strict';

    angular.module("nci.services.socket", [])
        .factory("socket", ["$window", "$q", function($window, $q) {
            return function socket(url) {
                var deferred, ws;

                deferred = $q.defer();
                try {
                    ws = new $window.WebSocket(url);
                    ws.addEventListener("open", function() {
                        deferred.resolve(this);
                    });
                    ws.addEventListener("close", function(event) {
                        deferred.reject(event);
                    });
                } catch (e) {
                    deferred.reject(e);
                }

                return deferred.promise;
            };
        }]);
})(angular);
