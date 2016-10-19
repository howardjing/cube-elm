var DashboardPlugin = require('webpack-dashboard/plugin');

module.exports = {
  entry: './src/index.js',

  output: {
    path: './dist',
    filename: 'index.js',
  },

  resolve: {
    modulesDirectories: ['node_modules'],
    extensions: ['', '.js', '.elm'],
  },

  plugins: [
    new DashboardPlugin()
  ],

  module: {
    loaders: [
      {
        test: /\.html$/,
        exclude: [/node_modules/],
        loader: 'file?name=[name].[ext]'
      },
      {
        test: /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/, /Stylesheets.elm/],
        loader: 'elm-webpack',
      },
      {
        test: /Stylesheets\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader: 'style!css!elm-css-webpack',
      },
      {
        test: /\.css/,
        excldue: [/node_modules/],
        loader: 'style!css',
      },
    ],
  },

  devServer: {
    inline: true,
    stats: 'errors-only',
  },
};