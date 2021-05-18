const { exec } = require('child_process');
const log = require('./log');
const deploy = process.env.DEPLOY;
// REQUIRE: `cabal new-install` to install `site` locally
exec('site rebuild', (err, stdout, stderr) => {
  log.tip(stdout);
  if (err) return log.error(stderr);

  if (!deploy) return

  log.info("[hakyll] prepared for deploying...")

  exec('node ./scripts/deploy.js', (err, stdout, stderr) => {
    log.tip(stdout);
    if (err) return log.error(stderr);

    log.info("[hakyll] deploy successfully!")
  });
});
