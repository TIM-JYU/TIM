//noinspection BadExpressionStatementJS
({
    mainConfigFile: 'main.js',
    baseUrl: '.',
    dir: '../build-optimized',
    paths: {
        'cs': 'empty:',
        'mcq': 'empty:',
        'mmcq': 'empty:',
        'svn': 'empty:',
        'tim/plugins': 'empty:',
        'tim/extramodules': 'empty:',
    },
    modules: [
        {
            name: 'tim/main',
        }
    ],
    generateSourceMaps: true,
    keepBuildDir: true,
    skipDirOptimize: true, // don't process unused JS files
    optimize: 'uglify',
});
