var Builder = require('systemjs-builder');
var builder = new Builder('.', 'jspm.config.js');
var realConsoleWarn = console.warn;

console.warn = function () {
    // hide TypeScript loader warning about using AMD module
    if (arguments.length < 3 || arguments[2].indexOf('consider setting module') < 0) {
        realConsoleWarn.apply(this, arguments);
    }
};

var bundles = [
    {
        modules: 'tim/main + tim/slide - [tim/**/*]', // all external dependencies
        file: 'build/deps.js'
    },
    {
        modules: 'tim/main + tim/slide - (tim/main + tim/slide - [tim/**/*])', // all TIM scripts without external dependencies
        file: 'build/tim.js'
    },
    {
        modules: 'tim/ace + ace/ext-language_tools', // preconfigured Ace editor
        file: 'build/ace.js'
    },
    {
        modules: 'tim/imagex - tim/main', // ImageX plugin
        file: 'build/imagex.js'
    }
];

//noinspection JSAnnotator
for (var bundle of bundles) {
    builder
        .bundle(bundle.modules,
            bundle.file,
            {minify: true, sourceMaps: true})
        .then(function (result) {
            console.log('Built ' + result.bundleName);
        })
        .catch(function (err) {
            console.error('Build error');
            console.error(err);
        });
}
