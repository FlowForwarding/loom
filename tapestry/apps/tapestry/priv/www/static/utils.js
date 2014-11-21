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
    function ip4ToNum(ip4) {
        // use 3 since it's ip4
        return ip4.split(".").reduce(function(res, bit, index) {
            return res + bit*Math.pow(256, 3 - index);
        }, 0);
    }

    function makeMask(n) {
        return (maxBits<<(32 - n)) & maxBits;
    }

    function parseNetwork(network) {
        var buf = network.split("/"),
            mask = makeMask(buf[1]),
            networkIPNum = ip4ToNum(buf[0]),
            networkRes = networkIPNum & mask;
        return {
            mask: mask,
            networkRes: networkRes
        }
    }

    var maxBits = Math.pow(2, 32) - 1,
        networks = ["10/8", "172.16/12", "192.168/16"].map(parseNetwork);

    ns.isExternal = function(endpoint) {
        var ip4NumAddress = ip4ToNum(endpoint),
            result = networks.reduce(function(val, network) {
                return val || ((ip4NumAddress & network.mask) == network.networkRes)
            }, false);

        return !result;
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

