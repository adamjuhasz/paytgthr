const createExpoWebpackConfigAsync = require("@expo/webpack-config");

module.exports = {
  stories: ["../src/**/*.stories.tsx"],
  addons: ["@storybook/addon-essentials"],
  webpackFinal: async function (config) {
    const c = await createExpoWebpackConfigAsync({ mode: "none" });

    config.module.rules.push({
      test: /\.(js|jsx|json|ts|tsx)$/,
      exclude: /node_modules[/\\](?!react-native-vector-icons|react-native-safe-area-view)/,
      use: {
        loader: "babel-loader",
        options: {
          presets: [
            "babel-preset-expo",
            "@babel/preset-env",
            "@babel/preset-react",
          ],
          plugins: [
            "@babel/plugin-proposal-class-properties",
            "@babel/plugin-proposal-object-rest-spread",
            "@babel/plugin-proposal-nullish-coalescing-operator",
            "@babel/plugin-proposal-optional-chaining",
          ],
        },
      },
    });
    config.resolve.alias = {
      ...config.resolve.alias,
      "react-native$": "react-native-web",
      "react-native/Libraries/Components/View/ViewStylePropTypes$":
        "react-native-web/dist/exports/View/ViewStylePropTypes",
      "react-native/Libraries/EventEmitter/RCTDeviceEventEmitter$":
        "react-native-web/dist/vendor/react-native/NativeEventEmitter/RCTDeviceEventEmitter",
      "react-native/Libraries/vendor/emitter/EventEmitter$":
        "react-native-web/dist/vendor/react-native/emitter/EventEmitter",
      "react-native/Libraries/vendor/emitter/EventSubscriptionVendor$":
        "react-native-web/dist/vendor/react-native/emitter/EventSubscriptionVendor",
      "react-native/Libraries/EventEmitter/NativeEventEmitter$":
        "react-native-web/dist/vendor/react-native/NativeEventEmitter",
      "@react-native-community/netinfo":
        "react-native-web/dist/exports/NetInfo",
      "react-native/Libraries/Image/AssetSourceResolver$":
        "expo-asset/build/AssetSourceResolver",
      "react-native/Libraries/Image/assetPathUtils$":
        "expo-asset/build/Image/assetPathUtils",
      "react-native/Libraries/Image/resolveAssetSource$":
        "expo-asset/build/resolveAssetSource",
      "react-native-svg": "react-native-svg-web",
    };
    return config;
  },
};
