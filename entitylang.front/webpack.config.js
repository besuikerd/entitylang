var path = require('path');
var webpack = require('webpack');
var ExtractTextPlugin = require('extract-text-webpack-plugin');
var autoprefixer = require('autoprefixer');

module.exports = {

  entry: {
    index: 'index.js'
  },

  output: {
    path: 'dist/',
    publicPath: 'http://localhost:3000/',
    filename: '[name].js',
    chunkFilename: '[chunkhash].js'
  },

  module: {
    loaders: [
      {
        test: /\.js$/,
        exclude: /node_modules/,
        loader: 'babel',
        query: {
          presets: ['es2015']
        }
      },

      {
        test: /(\.scss|\.css)$/,
        loader: ExtractTextPlugin.extract('style-loader', 'css-loader!postcss-loader!sass-loader')
      },

      {
        test: require.resolve('d3'),
        loader: 'imports-loader?d3'
      }
    ]
  },
  postcss: [autoprefixer],

  resolve: {
    modulesDirectories: [
      'node_modules',
      path.join(__dirname, 'js')
    ]
  },

  plugins: [
    new ExtractTextPlugin('[name].css'),
    new webpack.ProvidePlugin({
      d3: 'd3!'
    })
  ]
};
