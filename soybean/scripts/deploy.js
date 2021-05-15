const { exec } = require('child_process');
const env = require('../env.json');

const filename = `hakyll.${Date.now()}.tar`
exec(`cd ./_site && tar -cvjpf ${filename} *`, (err, stdout, stderr) => {
  console.log(stdout);
  console.log(stderr);
  if (!err) {
    exec(
      `scp ./_site/${filename} root@${env.ip}:${env.pkg}`,
      (err, stdout, stderr) => {
        console.log(stdout);
        console.log(stderr);
        if (err) {
          console.log('Failed to push to server', env.ip);
        } else {
          console.log('Scp successfully!');
          exec(
            `ssh -t root@${env.ip} 'node /home/scripts/hakyll.publish.js ${filename}'`,
            (err) => {
              if (err) {
                console.log(err);
              } else {
                console.log('Publish successfully!');
              }
            }
          );
        }
        exec(`rm ${filename}`);
      }
    );
  } else {
    exec(`rm ${filename}`);
    console.log("Failed to 'tar' your building result");
  }
});
