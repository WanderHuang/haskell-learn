const fn = require("child_process");

const map = {
  "css/tailwind-styles/default.css": "css/theme.css",
  "css/tailwind-styles/components.css": "css/components.css",
};

Object.entries(map).forEach(([key, val]) => {
  fn.execSync(`NODE_ENV=production tailwind build ${key} -o ${val}`);
});

