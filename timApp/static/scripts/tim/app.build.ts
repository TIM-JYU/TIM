//noinspection BadExpressionStatementJS,TsLint
({
    baseUrl: ".",
    dir: "../build-optimized",
    generateSourceMaps: true,
    keepBuildDir: true,
    mainConfigFile: "main.js",
    modules: [
        {
            name: "main",
        },
        {
            name: "views/frontpage",
        },
    ],
    optimize: "uglify",
    paths: {
        "simcir": "empty:",
        "simcir/basicset": "empty:",
        "simcir/library": "empty:",
        "simcir/oma-kirjasto": "empty:",
        "tim/angularmodules": "empty:",
        "tim/extramodules": "empty:",
        "tim/plugins": "empty:",
        "tim/session": "empty:",
        "tim/startupview": "empty:",
    },
    skipDirOptimize: true, // don't process unused JS files
    writeBuildTxt: false,
});
