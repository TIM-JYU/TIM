const createTimConfig = require("../../../eslint.config.tim");

module.exports = createTimConfig({
    tsconfigRootDir: __dirname,
    // ESLint would otherwise take the parser and the typescript-eslint plugin from timApp. The two directories
    // have different TypeScript versions, and rules that use type information silently stop working
    // if the parser and the plugin do not share the same TypeScript.
    parser: require("@typescript-eslint/parser"),
    tsPlugin: require("@typescript-eslint/eslint-plugin"),
    ignores: ["**/*.js", "dist/", "build/"],
});
