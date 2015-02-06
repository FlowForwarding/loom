(function (angular) {
    'use strict';

    angular.module('nci.components.services.endpointsService', [
        'ngMaterial'
    ])
        .factory("nciEndpointsDialog", ["$mdDialog", function($mdDialog) {
            var dialog;
            return {
                show: function(title) {
                    var alert = $mdDialog.alert()
                        .title(title)
                        .content('Activity endpoints will be here')
                        .ok('Close');

                    return $mdDialog
                        .show( alert )
                        .finally(function() {
                            alert = undefined;
                        });
                },
                hide: function() {

                }
            };
        }]);

})(angular);
