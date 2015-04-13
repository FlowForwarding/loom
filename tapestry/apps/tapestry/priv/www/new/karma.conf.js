'use strict';
module.exports = function(config){
  config.set({

    basePath : './',

    files : [
      'app/bower_components/angular/angular.js',
      'app/bower_components/angular-route/angular-route.js',
      'app/bower_components/angular-mocks/angular-mocks.js',
//      'app/services/*.js',
//      'app/components/**/*.js',
//      'app/view*/**/*.js',
      'socketMock/*.js'
    ],

    autoWatch : true,

    frameworks: ['jasmine'],

    browsers : ['Chrome'],

    plugins : [
            'karma-chrome-launcher',
            'karma-firefox-launcher',
            'karma-jasmine',
            'karma-junit-reporter',
            'karma-js-coverage'
            ],

    junitReporter : {
      outputFile: 'test_out/unit.xml',
      suite: 'unit'
    },

    preprocessors: {
        'socketMock/!(*_test).js': 'coverage'
    },

    reporters: ['coverage'],

    coverageReporter: {
        type : 'lcov',
        dir : 'coverage/'
    }

  });
};
