const log = require("./log");
const fn = require("child_process");
const watch = require("node-watch");

let app;

try {
  log.info("Building...");
  // need `cabal new-install` run at first
  fn.exec("site rebuild", (err, _, stderr) => {
    log.info("Building Success ! We can start now.");

    if (err) throw new Error(stderr);
    log.info("$ Watching Soybean: http://localhost:8000/ $");
    app = fn.exec("site watch", (err, _, stderr) => {
      if (err) throw new Error(stderr);
    });
  });
} catch (err) {
  log.error("xxx", err);
  process.exit(0);
}

watch(
  "css",
  {
    recursive: true,
    filter: (f, skip) => {
      if (!/default\.css$/.test(f)) return skip;

      return /\.css$/.test(f);
    },
  },
  (_, name) => {
    fn.exec("npm run build:css", (err, _, stderr) => {
      if (err) return;
      log.info(new Date(), name, "call rebuilding");
    });
  }
);

watch("templates", { recursive: true }, (_, name) => {
  log.info(new Date(), name, "call rebuilding");
  fn.exec("site rebuild", (err, _, stderr) => {
    if (err) return log.error(err);
    log.info(new Date(), name, "rebuilding successfully");
  });
});

/*
watch("site.hs", { recursive: false }, (_, name) => {
  log.info(new Date(), name, "call reinstall");
  fn.exec("cabal --overwrite-policy=always new-install", (err, _, stderr) => {
    if (err) return log.error(err);

    log.info(new Date(), name, "reinstall successfully");
    log.info(new Date(), name, "call rebuilding");

    fn.exec("site rebuild", (err, _, stderr) => {
      if (err) return log.error(stderr);

      log.info(new Date(), name, "rebuilding successfully");
    });
  });
});
*/
