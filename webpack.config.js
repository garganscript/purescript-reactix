'use strict';
let path = require('path');
let exec = require('executive');
let nodeExternals = require('webpack-node-externals');
let isWebpackDevServer = process.argv.some(a => path.basename(a) === 'webpack-dev-server');
let HtmlWebpackPlugin = require('html-webpack-plugin');
let CleanWebpackPlugin = require('clean-webpack-plugin');
let isWatch = process.argv.some(a => a === '--watch');

let spago_sources = async () =>
  exec.quiet(
    "spago sources",
    { options: 'strict' }
  ).then(function (res) {
    let sources = res.stdout.split(/\r?\n/);
    sources.pop(); // extra newline at the end of output
    return sources;
  });


let dist = path.join(__dirname, 'dist');
let src = path.join(__dirname, 'src');
let test = path.join(__dirname, 'test');

module.exports = (env) =>
  spago_sources()
  .then(function (ps_sources) {

    ps_sources.push('src/**/*.purs');
    if (env === "test")
      ps_sources.push('test/**/*.purs');

    let config = {
      mode: 'development',
      target: "web",
      devtool: 'inline-source-map',
      devServer: {
        disableHostCheck: true,
        contentBase: dist,
        compress: true,
        port: 9000
      },
      output: {
        path: dist,
        filename: 'bundle.js'
      },
      module: {
        rules: [
          {test: /\.purs$/,
           exclude: /(node_modules)/,
           use: [
             {loader: "purs-loader",
              options: {
                src: ps_sources,
                output: dist,
                pscIde: true,
                pscIdeClientArgs: {port: 4002},
                pscIdeServerArgs: {port: 4002},
                pscPackage: false,
                bundle: false,
                watch: isWatch           
              }}
           ]},
          {test: /\.css$/,
           exclude: /(node_modules)/,
           use: [
             "style-loader",
             "css-loader",
             {loader: "sass-loader",
              options: {
                includePaths: ["src"]
              }}
           ]},
          {test: /\.(png|jpg|gif|svg)$/,
           exclude: /(node_modules)/,
           use: [ "file-loader" ]},
          {test: /\.js$/,
           exclude: /(node_modules)/,
           use: {
             loader: "babel-loader",
           }
          }
        ]
      },
      resolve: {
        modules: [ 'node_modules' ],
        extensions: [ '.purs', '.js']
      },
      plugins: [
        new CleanWebpackPlugin(['dist']),
      ],
      entry: path.join(test, "test.js")
      // externals: [nodeExternals()]
    };
    switch(env) {
    case 'browser':
      config.plugins.push(new HtmlWebpackPlugin({
        title: "Reactix",
        template: path.join(test, "browser.html")
      }));
      break;
    case 'dev': break;
    case 'headless': break;
    default:
      console.log("unknown env: ", env);
    }
    return config;
  });
