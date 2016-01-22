(function (angular) {
    'use strict';

    angular.module('nci.services.utils', [])
        .factory('wildcardStringToRegExp', function() {
            return function wildcardStringToRegExp(str) {
                // converts string with wildcards to regex
                // * - zero or more
                // ? - exact one

                str = str.replace(/\./g, "\\.");
                str = str.replace(/\?/g, ".");
                str = str.replace(/\*/g, ".*");

                return new RegExp(str);
            };
        });

})(angular);
