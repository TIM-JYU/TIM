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
