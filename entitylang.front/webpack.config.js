var path = require('path');
var webpack = require('webpack');

var ExtractTextPlugin = require('extract-text-webpack-plugin');

module.exports = {

    entry: 'index',

    output: {
        path: path.join(__dirname, 'dist/'),
        filename: '[name].js'
    },

    module: {
        rules: [
            {
                test: /\.(css|scss)$/,
                use: ExtractTextPlugin.extract({
                    fallback: 'style-loader'
                    , use: ['css-loader', 'sass-loader']
                })
            },

            {
                test: /\.(eot|woff|woff2|ttf|svg|png|jpe?g|gif)(\?\S*)?$/,
                use: [
                    {
                        loader:  'url-loader',
                        options: {
                            limit: 100000,
                            name: '[name].[ext]'
                        }
                    }
                ]
            }
        ]
    },

    devtool: 'inline-source-map',
    resolve: {
        modules: [
            path.join(__dirname, 'node_modules'),
            path.join(__dirname, 'js')
        ]
    },

    plugins: [
        new ExtractTextPlugin('[name].css')
      , new webpack.ProvidePlugin({
            d3: 'd3'
        })
    ],

    devServer: {
        publicPath: '/target',
        port: 9000
    }

};