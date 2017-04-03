//noinspection BadExpressionStatementJS,TsLint
({
    baseUrl: ".",
    dir: "../build-optimized",
    generateSourceMaps: true,
    keepBuildDir: true,
    mainConfigFile: "main.js",
    modules: [
        {
            name: "tim/main",
        },
        {
            name: "ace/ace",
        },
        {
            name: "ace/snippets",
        },
        {
            name: "ui-grid",
        },
    ],
    optimize: "uglify",
    paths: {
        "tim/angularmodules": "empty:",
        "tim/extramodules": "empty:",
        "tim/plugins": "empty:",
        "tim/session": "empty:",
    },
    skipDirOptimize: true, // don't process unused JS files
    writeBuildTxt: false,
});
