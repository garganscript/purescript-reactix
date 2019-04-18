// Karma configuration
// Generated on Thu Feb 21 2019 19:31:16 GMT+0100 (Central European Standard Time)

module.exports = function(config) {
  config.set({
    basePath: '',
    frameworks: ['mocha'],
    files: [ "output/bundle.js" ],
    plugins: [ 'karma-chrome-launcher' , 'karma-mocha'],
    exclude: [],
    preprocessors: {},
    reporters: ['progress'],
    port: 9876,
    colors: true,
    // possible values: config.LOG_DISABLE || config.LOG_ERROR || config.LOG_WARN || config.LOG_INFO || config.LOG_DEBUG
    logLevel: config.LOG_INFO,
    autoWatch: false,
    browsers: ['Chrome'],
    singleRun: true,
    concurrency: 1
  })
}
