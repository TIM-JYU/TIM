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
    ],
    optimize: "uglify",
    paths: {
        "cs": "empty:",
        "mcq": "empty:",
        "mmcq": "empty:",
        "svn": "empty:",
        "tim/extramodules": "empty:",
        "tim/plugins": "empty:",
        "tim/session": "empty:",
    },
    skipDirOptimize: true, // don't process unused JS files
});
