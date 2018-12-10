const merge = require("webpack-merge")
const base = require("./webpack.common.js")
const MiniCssExtractPlugin = require("mini-css-extract-plugin")
const HTMLWebpackPlugin = require("html-webpack-plugin");

// Spins up dev server with bundles using minimal template
module.exports = merge(base, {
    mode: "development",
    output: {
        filename: "[name].dev.[hash].js"
    },
    devServer: {
        inline: true,
        stats: "errors-only",
        contentBase: ["./static", "./static-dev"],
        historyApiFallback: true
    },
    module: {
        rules: [
            {
                test: /\.less$/,
                use: [MiniCssExtractPlugin.loader, "css-loader", "less-loader"]
            },
            {
                test: /\.css$/,
                use: [MiniCssExtractPlugin.loader, "css-loader"]
            },
            {
                test: /\.elm$/,
                exclude: [/elm-stuff/, /node_modules/],
                use: [
                    { loader: "elm-hot-webpack-loader" },
                    {
                        loader: "elm-webpack-loader",
                        options: {
                            // add Elm's debug overlay to output
                            debug: true,
                            forceWatch: true
                        }
                    }
                ]
              }
        ]
    },
    plugins: [
        new MiniCssExtractPlugin({
            filename: "[name].dev.[chunkhash].css"
        }),
        new HTMLWebpackPlugin({
            // Use this template to get basic responsive meta tags
            template: "src/index.html",
            // inject details of output file at end of body
        })
    ]
})
