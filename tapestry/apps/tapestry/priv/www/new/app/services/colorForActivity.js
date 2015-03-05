(function(angular) {
    'use strict';

    angular.module("nci.services.colorForActivity", [])
        .factory("colorForActivity", ["colors", function(colors) {
            colors = colors.series;

            function normalizeIndex(index) {
                return index % colors.length;
            }
            return function(activity) {
                return colors[normalizeIndex(activity.index)];
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
                INTERNAL_OVER_EXTERNAL: "#2ca02c",
                MAX_INTERNAL_FLOWS: "#69456f"
            },
            endpoints: {
                'default': "",
                EXTERNAL_EXTERNAL_CONNECTIONS: "#581A00",
                INTERNAL_EXTERNAL_CONNECTIONS: "#ff9896",
                EXTERNAL_INTERNAL_CONNECTIONS: "darkgreen",
                INTERNAL_INTERNAL_CONNECTIONS: "#a1d99b",
                OUTSIDE_ACTIVITY: "red",
                EXTERNAL: "#69456f"
            }
        });
})(angular);