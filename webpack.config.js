const webpack = require('webpack');

module.exports = {
    context: __dirname,
    devtool: 'source-map',
    entry: './src/main.ts',
    output: {
        path: __dirname+'/build',
        filename: 'bundle.js'
    },
    resolve: {
        extensions: ['.js', '.ts']
    },
    module: {
        rules: [{ test: /\.ts$/, loader: 'awesome-typescript-loader' }]
    },
    plugins: [
        new webpack.optimize.UglifyJsPlugin({ minimize: true, sourceMap: true })
    ]
};
