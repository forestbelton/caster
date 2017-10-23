var path = require('path')
var webpack = require('webpack')

module.exports = {
    entry: './index.js',
    output: {
        path: path.resolve(__dirname, 'docs'),
        filename: 'bundle.js'
    },
    devServer: {
        contentBase: path.resolve(__dirname, 'docs')
    },
    module: {
        loaders: [
            {
                test: /\.purs$/,
                exclude: /node_modules/,
                loader: 'purs-loader',
                query: {
                    src: ['bower_components/purescript-*/src/**/*.purs', 'lib/**/*.purs']
                }
            }
        ]
    }
}