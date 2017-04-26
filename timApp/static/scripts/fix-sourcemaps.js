const replace = require('replace-in-file');

// Fixes SystemJS builder generated source maps.
replace.sync({
    files: 'node_modules/systemjs-builder/lib/sourcemaps.js',
    from: 'fromFileURL(source)',
    to: 'fromFileURL(source.replace("file:///file:/", "file:///"))'
});
