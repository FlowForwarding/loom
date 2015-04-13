(function(angular) {
    'use strict';

    angular.module('nci.services.nciIsExternal', [])
        .factory("checkExternal", function() {
            var IPv4RangeList = {
                    internalIPv4Network: [
                        [ipaddr.parse('10.0.0.0'), 8],
                        [ipaddr.parse('172.16.0.0'), 12],
                        [ipaddr.parse('192.168.0.0'), 16]
                    ]
                },
                IPv6RangeList = {
                    internalIPv6Network: [
                        [ipaddr.parse('fc00::'), 7]
                    ]
                },
                externalSubnet = "external";

            return function(endpoint) {
                var addr = ipaddr.parse(endpoint),
                    rangeList = addr.kind() == "ipv4" ? IPv4RangeList : IPv6RangeList;
                return ipaddr.subnetMatch(addr, rangeList, externalSubnet) == externalSubnet;
            };
        });

})(angular);
