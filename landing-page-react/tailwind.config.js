const colors = require("tailwindcss/colors");
const defaultSans = require("tailwindcss/defaultTheme").fontFamily.sans;

module.exports = {
  purge: ["./src/**/*.{js,jsx,ts,tsx}", "./public/index.html"],
  darkMode: false, // or 'media' or 'class'
  modee: "jit",
  theme: {
    extend: {
      colors: {
        rose: colors.rose,
      },
      fontFamily: {
        sans: ["Rubik", ...defaultSans],
      },
    },
  },
  variants: {
    extend: {},
  },
  plugins: [
    require("@tailwindcss/forms"),
    require("@tailwindcss/typography"),
    require("@tailwindcss/aspect-ratio"),
  ],
};
