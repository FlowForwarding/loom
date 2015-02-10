(function(angular) {
    'use strict';

    angular.module("nci.services.nciEndpointModel", [
        "nci.services.model"
    ])
        .factory("endpointsPromise", ["modelPromise", function(modelPromise) {
            return modelPromise
                .then(function(model) {
                    return {
                        //byIndex: function (index) {
                        //    return model.activities()[index - 1];
                        //},
                        byIp: function (ip) {
                            return model.getEndpointByIp(ip);
                        },
                        all: function () {
                            return model.endpoints();
                        }
                    };
                });
        }]);
})(angular);
