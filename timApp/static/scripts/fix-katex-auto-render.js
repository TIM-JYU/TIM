const replace = require('replace-in-file');

// Fixes KaTeX auto-render extension so that it will throw exceptions instead of printing to console.
replace.sync({
    files: 'jspm_packages/npm/katex@0.7.1/dist/contrib/auto-render.min.js',
    from: 'f instanceof katex.ParseError',
    to: 'false'
});
