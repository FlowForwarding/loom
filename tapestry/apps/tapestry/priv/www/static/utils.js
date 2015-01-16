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


/**
 *
 */

(function(ns) {
    function downloadFile(fileType, fileContent, fileName) {
        var blob = new Blob([fileContent], {type: fileType}),
            url = URL.createObjectURL(blob),
            $a = $("<a>")
                .attr("href", url)
                .attr("download", fileName)
                .attr("target", "_blank");

        // This timeout is to fix issue with Safari download
        setTimeout(function() {
            $a
                .get(0)
                .click();
            // need timeout to have time to open file before we revoke it from memory
            setTimeout(URL.revokeObjectURL.bind(URL, url), 100);
        }, 0);
    }

    ns.downloadFile = downloadFile;

})(NCI.utils);

/**
 *
 */
(function(ns) {
    ns.csv = {}
    function createCSV(columns, data) {
        var csvRows = [columns.map(function(item) {
            return ['"',
                item.text.replace('"', '""'),
                '"'].join("");
        }).join(",")];

        csvRows = csvRows.concat(data.map(function(item) {
            return columns.map(function(column) {
                var res = item[column.property];
                if (column.renderer) {
                    res = column.renderer(res);
                }
                return res;
            }).join(",");
        }));

        return csvRows.join("\n");
    }

    ns.csv.create = createCSV;
    ns.csv.download = NCI.utils.downloadFile.bind(null, "text/csv");


})(NCI.utils);
