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
    filename: '[name].bundle.js',
    path: path.resolve(__dirname, 'dist'),
  },

  mode: 'development',

  devServer: {
      contentBase: path.resolve(__dirname, 'dist'),
      writeToDisk: true,
      open: true,
  },

  plugins: [
      new CopyPlugin({
          patterns: [
              {
                  from: 'index.html',
              },
              {
                  from: 'assets/**/*',
              },
          ]
      }),
      new webpack.DefinePlugin({
          'typeof CANVAS_RENDERER': JSON.stringify(true),
          'typeof WEBGL_RENDERER': JSON.stringify(true),
      }),
  ],
}
