(function(angular) {
    'use strict';

    angular.module("nci.services.colorForActivity", [])
        .factory("colorForActivity", ["colors", function(colors) {
            colors = colors.series;

            function normalizeIndex(index) {
                return index % colors.length;
            }
            return function(activity) {
                return activity ? colors[normalizeIndex(activity.index)] : "#000000";
            };
        }])
        .value("colors", {
            series: [
                "#1f77b4",
                "#aec7e8",
                "#ff7f0e",
                "#ffbb78",
                "#2ca02c",
                "#98df8a",
                "#d62728",
                "#ff9896",
                "#9467bd",
                "#c5b0d5",
                "#8c564b",
                "#c49c94",
                "#e377c2",
                "#f7b6d2",
                "#7f7f7f",
                "#c7c7c7",
                "#bcbd22",
                "#dbdb8d",
                "#17becf",
                "#9edae5"
            ],
            activities: {
                'default': "#1f77b4",
                MAX_INTERNAL_FLOWS: "#2ca02c",
                INTERNAL_OVER_EXTERNAL_ENDPOINTS: "#69456f"
            },
            endpoints: {
                'default': "#1f77b4",
                // ENDPOINT_CONNECTIONS
                //EXTERNAL_EXTERNAL_CONNECTIONS: "#333399",
                //EXTERNAL_INTERNAL_CONNECTIONS: "#999933",
                EXTERNAL_EXTERNAL_CONNECTIONS: "#512DA8",
                EXTERNAL_INTERNAL_CONNECTIONS: "#FFEB3B",
                //EXTERNAL_EXTERNAL_CONNECTIONS: "#581A00",
                //EXTERNAL_INTERNAL_CONNECTIONS: "darkgreen",
                INTERNAL_EXTERNAL_CONNECTIONS: "#FF5722",
                INTERNAL_INTERNAL_CONNECTIONS: "#689F38",
                //INTERNAL_EXTERNAL_CONNECTIONS: "#ff9896",
                //INTERNAL_INTERNAL_CONNECTIONS: "#a1d99b",
                OUTSIDE_ACTIVITY: "red",
                EXTERNAL: "#69456f",
                EXTERNAL_WHITE: "sandybrown"
            }
        });
})(angular);