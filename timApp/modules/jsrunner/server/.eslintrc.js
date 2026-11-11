module.exports = {
    root: true,
    extends: "../../../.eslintrc.js",
    // ESLint loads plugins from this directory but would take the parser from the extended configuration's
    // directory (timApp). The two have different TypeScript versions, and rules that use type information
    // silently stop working if the parser and the plugins do not share the same TypeScript.
    parser: require.resolve("@typescript-eslint/parser"),
    parserOptions: {
        "project": "tsconfig.json",
        "sourceType": "module"
    },
};
