const replace = require('replace-in-file');

// Fixes some issues with JSPM watcher:
// - watchman is off by default
// - config files are excluded from the set of watched files because otherwise the underlying fs.watch call will also
//   watch node_modules and jspm_packages that have lots of files
// - fixes apparently incorrect value passed as glob
// - uses polling (it is more reliable; the non-poll method sometimes misses changes)
replace.sync({
    files: 'node_modules/jspm/lib/bundle.js',
    from: [
        'files = files.concat(configFiles);',
        'var watchman = true;',
        'glob: watchman && watchFiles'
    ],
    to: [
        'files = files.concat([]);',
        'var watchman = false;',
        'glob: relFiles, poll: true, interval: 500'
    ]
});

// Fixes async-await syntax so that uncaught exceptions are logged.
replace.sync({
    files: 'jspm_packages/npm/tslib@1.9.3/tslib.js',
    from: /catch \(e\) { reject\(e\); }/g,
    to: 'catch (e) { console.log(e); reject(e); }'
});

// Fixes KaTeX auto-render extension so that it will throw exceptions instead of printing to console.
replace.sync({
    files: 'jspm_packages/npm/katex@0.7.1/dist/contrib/auto-render.min.js',
    from: 'f instanceof katex.ParseError',
    to: 'false'
});
