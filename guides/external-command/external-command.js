const { execSync } = require("node:child_process");

const output = execSync("hh200 --version", {
  encoding: "utf8",
});

console.log(output);
