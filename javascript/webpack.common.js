const path = require("path");
const webpack = require('webpack');
const CopyPlugin = require('copy-webpack-plugin');

module.exports = {
  entry: {
      app: './src/index.ts',
  },

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      },
    ],
  },
  devtool: 'inline-source-map',

  resolve: {
    extensions: [ '.tsx', '.ts', '.js' ],
  },

  output: {
    filename: 'app.bundle.js',
    path: path.resolve(__dirname, 'dist'),
    library: {
      name: 'app',
      type: 'umd'
    },
  },

  mode: 'development',

  devServer: {
      contentBase: path.resolve(__dirname, 'dist'),
      writeToDisk: true,
      open: true,
  },

  plugins: [],
}
