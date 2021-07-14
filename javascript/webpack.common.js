const path = require("path");
const webpack = require('webpack');
const CopyPlugin = require('copy-webpack-plugin');

module.exports = {
  entry: {
    app: './src/index.ts',
    demo: './src/demo.ts'
  },

  module: {
    rules: [
      {
        test: /\.tsx?$/,
        use: 'ts-loader',
        exclude: /node_modules/,
      }
    ],
  },
  devtool: 'inline-source-map',

  resolve: {
    extensions: [ '.tsx', '.ts', '.js' ],
  },

  output: {
    filename: '[name].bundle.js',
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

  performance: {
    hints: false,
    maxEntrypointSize: 512000,
    maxAssetSize: 512000
  }
}
