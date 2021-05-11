let chalk = require("chalk");

const log = (fn) => (...str) => console.log(fn(...str));

const info = log(chalk.cyan);
const warn = log(chalk.yellow);
const error = log(chalk.red);
const tip = log((str) => str);

module.exports = { info, warn, error, tip };
