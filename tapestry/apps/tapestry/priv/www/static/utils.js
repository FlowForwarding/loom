/**
 * utils.js
 * this file is for some useful code parts which could be used across all application
 */

if (typeof NCI === 'undefined') {
    NCI = {};
}

NCI.utils = {};

/**
 * External networks checker
 */
(function(ns) {
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

    ns.isExternal = function(endpoint) {
        var addr = ipaddr.parse(endpoint),
            rangeList = addr.kind() == "ipv4" ? IPv4RangeList : IPv6RangeList;
        return ipaddr.subnetMatch(addr, rangeList, externalSubnet) == externalSubnet;
    };

})(NCI);

/**
 *
 */
(function(ns) {
    ns.wildcardStringToRegExp = function wildcardStringToRegExp(str) {
        // converts string with wildcards to regex
        // * - zero or more
        // ? - exact one

        str = str.replace(/\./g, "\\.");
        str = str.replace(/\?/g, ".");
        str = str.replace(/\*/g, ".*");

        return new RegExp(str);
    }

})(NCI.utils);

