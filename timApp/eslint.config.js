const createTimConfig = require("./eslint.config.tim");

module.exports = createTimConfig({
    tsconfigRootDir: __dirname,
    ignores: [
        // Only TypeScript is linted.
        "**/*.js",
        "**/*.mjs",
        "**/*.cjs",
        "modules/cs/static/mindsim/",
        "modules/cs/simcir/",
        "modules/jsrunner/server/",
        "modules/angjsrunner/",
        "static/scripts/tim/environments/environment.prod.ts",
        "**/*.worker.ts",
        "i18n/",
    ],
});
