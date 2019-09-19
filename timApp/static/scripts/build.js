"use strict";
var Builder = require('systemjs-builder');
var builder = new Builder('.', 'jspm.config.js');

var bundles = [
    {
        modules: 'tim/main - [tim/**/*] - reveal + jquery + tslib', // all non-lazy external dependencies
        file: 'build/deps.js'
    },
    {
        modules: 'chart.js - moment', // Chart.js, lazily loaded
        file: 'build/chart.js'
    },
    {
        modules: 'katex + katex-auto-render', // KaTeX, lazily loaded
        file: 'build/katex.js'
    },
    {
        modules: 'mathjax', // MathJax, lazily loaded
        file: 'build/mathjax.js'
    },
    {
        modules: 'tim/main - (tim/main - [tim/**/*])', // all TIM scripts without external dependencies
        file: 'build/tim.js'
    },
    {
        modules: 'tim/editor/ace', // preconfigured Ace editor, lazily loaded
        file: 'build/ace.js'
    },
];

//noinspection JSAnnotator
for (let bundle of bundles) {
    builder
        .bundle(bundle.modules,
            bundle.file,
            {minify: true, sourceMaps: true})
        .then(function (result) {
            bundle.sources = [];
            //noinspection JSAnnotator
            for (let key of Object.keys(result.tree)) {
                var obj = result.tree[key];
                var info;
                if (obj === false) {
                    info = "excluded; will be fetched at runtime";
                    bundle.sources.push({file: key, size: 0, info: info});
                } else {
                    var source = obj.source;
                    info = source ? source.length + " bytes" : "no source field; skipping";
                    bundle.sources.push({file: key, size: source ? source.length : -1, info: info});
                }
            }
            bundle.sources.sort(function (a, b) {
                return b.size - a.size;
            });
            //noinspection JSAnnotator
            for (let s of bundle.sources) {
                console.log(s.file + " (" + s.info + ")");
            }
            console.log('Bundled the above files to: ' + result.bundleName);
            console.log();
        })
        .catch(function (err) {
            console.error('Build error');
            console.error(err);
        });
}
