const colors = require("tailwindcss/colors");

module.exports = {
  prefix: "yc-",
  purge: [
    "./posts/**/*.html",
    "./templates/**/*.html",
    "*.html",
    "./posts/**/*.md",
    "./templates/**/*.md",
    "*.md",
  ],
  darkMode: "class",
  theme: {
    screens: {
      sm: "480px",
      md: "768px",
      lg: "976px",
      xl: "1440px",
    },
    colors: {
      gray: colors.coolGray,
      blue: colors.lightBlue,
      red: colors.rose,
      pink: colors.fuchsia,
      purple: colors.purple,
      cyan: colors.cyan,
      black: colors.black,
      green: colors.green,
      white: colors.white,
      orange: colors.orange,
      indigo: colors.indigo,
      yellow: colors.yellow,
    },
    fontFamily: {
      sans: ["Graphik", "sans-serif"],
      serif: ["Merriweather", "serif"],
    },
    extend: {
      spacing: {
        128: "32rem",
        144: "36rem",
      },
      borderRadius: {
        "4xl": "2rem",
      },
    },
  },
};
