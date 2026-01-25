// preinstall.js - Check Node.js version compatibility

const MIN_NODE_VERSION = 18;

const nodeVersion = process.versions.node;
const major = parseInt(nodeVersion.split('.')[0], 10);

if (major < MIN_NODE_VERSION) {
  console.error(`hanzo-node requires Node.js ${MIN_NODE_VERSION} or higher.`);
  console.error(`Current version: ${nodeVersion}`);
  console.error(`Please upgrade Node.js: https://nodejs.org/`);
  process.exit(1);
}
