SystemJS.config({
  paths: {
    "github:": "jspm_packages/github/",
    "npm:": "jspm_packages/npm/",
    "simcir": "/cs/simcir/simcir.js",
    "simcir/basicset": "/cs/simcir/simcir-basicset.js",
    "simcir/library": "/cs/simcir/simcir-library.js",
    "simcir/oma-kirjasto": "/cs/simcir/simcir-oma-kirjasto.js",
    "mathjax": "mathjax/mathjax.js",
    "sagecell": "/static/embedded_sagecell.js",
    "cs/": "/cs/js/build/",
    "svn/": "/svn/js/build/",
    "tim/": "tim/"
  },
  browserConfig: {
    "baseURL": "/static/scripts"
  },
  devConfig: {
    "map": {
      "core-js": "npm:core-js@1.2.7",
      "ts-runtime": "npm:babel-runtime@5.8.38",
      "plugin-typescript": "github:frankwallis/plugin-typescript@9.0.0",
      "source-map-support": "npm:source-map-support@0.5.12"
    },
    "packages": {
      "npm:babel-runtime@5.8.38": {
        "map": {}
      },
      "npm:core-js@1.2.7": {
        "map": {}
      },
      "npm:source-map-support@0.5.12": {
        "map": {
          "buffer-from": "npm:buffer-from@1.1.1",
          "source-map": "npm:source-map@0.6.1"
        }
      }
    }
  },
  transpiler: "plugin-typescript",
  babelOptions: {
    "optional": [
      "runtime",
      "optimisation.modules.system"
    ]
  },
  typescriptOptions: {
    "module": "system",
    "noImplicitAny": false,
    "noImplicitThis": false,
    "allowUnreachableCode": true,
    "removeComments": true,
    "preserveConstEnums": true,
    "sourceMap": true,
    "allowJs": true,
    "moduleResolution": "node",
    "baseUrl": ".",
    "target": "es5",
    "lib": [
      "dom",
      "es7",
      "scripthost"
    ],
    "downlevelIteration": true,
    "importHelpers": true
  },
  packages: {
    "tim": {
      "defaultExtension": "ts",
      "main": "main.ts",
      "meta": {
        "*.ts": {
          "loader": "ts"
        }
      }
    },
    "cs": {
      "defaultExtension": "js"
    },
    "/jsrunner": {
      "defaultExtension": "js"
    },
    "/field": {
      "defaultExtension": "js"
    }
  },
  meta: {
    "*.css": {
      "loader": "css"
    },
    "npm:angular-eonasdan-datetimepicker@0.3.9/dist/angular-eonasdan-datetimepicker.js": {
      "deps": [
        "angular",
        "eonasdan-bootstrap-datetimepicker"
      ],
      "globals": {
        "moment": "npm:moment@2.24.0/moment.js"
      }
    },
    "npm:angular@1.7.5/angular.js": {
      "deps": [
        "jquery",
        "eonasdan-bootstrap-datetimepicker"
      ]
    },
    "npm:angular-timer@1.3.5/dist/angular-timer.js": {
      "deps": [
        "angular"
      ],
      "globals": {
        "humanizeDuration": "github:EvanHahn/HumanizeDuration.js@3.15.1/humanize-duration.js",
        "moment": "npm:moment@2.24.0/moment.js"
      }
    },
    "npm:katex@0.7.1/dist/contrib/auto-render.min.js": {
      "globals": {
        "katex": "npm:katex@0.7.1/katex.js"
      }
    },
    "npm:angular-diff-match-patch@0.7.4/angular-diff-match-patch.js": {
      "globals": {
        "diff_match_patch": "npm:diff-match-patch@1.0.4/index.js"
      }
    },
    "/cs/simcir/simcir-basicset.js": {
      "deps": [
        "simcir"
      ],
      "format": "global"
    },
    "/cs/simcir/simcir-library.js": {
      "deps": [
        "simcir"
      ],
      "format": "global"
    },
    "/cs/simcir/simcir-oma-kirjasto.js": {
      "deps": [
        "simcir"
      ],
      "format": "global"
    },
    "/static/scripts/mathjax/mathjax.js": {
      "format": "global",
      "exports": "MathJax"
    },
    "/static/embedded_sagecell.js": {
      "globals": {
        "MathJax": "https://cdn.rawgit.com/pkra/MathJax-single-file/master/dist/TeXSVGTeX/MathJax.js"
      },
      "exports": "sagecell"
    }
  },
  map: {
    "katex-auto-render": "npm:katex@0.7.1/dist/contrib/auto-render.min",
    "reveal": "npm:reveal.js@3.8.0"
  },
  bundles: {
    "build/ace.js": [
      "tim/editor/ace.ts",
      "github:ajaxorg/ace-builds@1.4.5/ext-language_tools.js"
    ],
    "build/chart.js": [
      "npm:chart.js@2.7.2/src/chart.js"
    ],
    "build/katex.js": [
      "npm:katex@0.7.1/katex.js",
      "npm:katex@0.7.1/dist/contrib/auto-render.min.js"
    ]
  }
});

SystemJS.config({
  packageConfigPaths: [
    "npm:@*/*.json",
    "npm:*.json",
    "github:*/*.json"
  ],
  map: {
    "angular-drag-and-drop-lists": "npm:angular-drag-and-drop-lists@2.1.0",
    "mobile-drag-drop": "npm:mobile-drag-drop@2.3.0-rc.1",
    "css": "github:systemjs/plugin-css@0.1.37",
    "deepmerge": "npm:deepmerge@3.0.0",
    "io-ts": "npm:io-ts@1.9.0",
    "angular-diff-match-patch": "npm:angular-diff-match-patch@0.7.4",
    "ifvisible.js": "npm:ifvisible.js@1.0.6",
    "angularjs-color-picker": "npm:angularjs-color-picker@3.4.8",
    "angular-ui-grid": "npm:angular-ui-grid@4.6.3",
    "ace": "github:ajaxorg/ace-builds@1.4.5",
    "constants": "npm:jspm-nodelibs-constants@0.2.1",
    "events": "npm:jspm-nodelibs-events@0.2.2",
    "angular": "npm:angular@1.7.5",
    "angular-animate": "npm:angular-animate@1.7.3",
    "angular-bootstrap-colorpicker": "npm:angular-bootstrap-colorpicker@3.0.26",
    "angular-eonasdan-datetimepicker": "npm:angular-eonasdan-datetimepicker@0.3.9",
    "angular-messages": "npm:angular-messages@1.7.3",
    "angular-sanitize": "npm:angular-sanitize@1.7.3",
    "angular-timer": "npm:angular-timer@1.3.5",
    "angular-touch": "npm:angular-touch@1.7.3",
    "angular-ui-bootstrap": "npm:angular-ui-bootstrap@2.5.0",
    "assert": "npm:jspm-nodelibs-assert@0.2.1",
    "bootstrap": "github:twbs/bootstrap@3.3.7",
    "bootstrap-sass": "github:twbs/bootstrap-sass@3.3.7",
    "buffer": "npm:jspm-nodelibs-buffer@0.2.3",
    "chart.js": "npm:chart.js@2.8.0",
    "child_process": "npm:jspm-nodelibs-child_process@0.2.1",
    "crypto": "npm:jspm-nodelibs-crypto@0.2.1",
    "eonasdan-bootstrap-datetimepicker": "npm:eonasdan-bootstrap-datetimepicker@4.17.47",
    "fs": "npm:jspm-nodelibs-fs@0.2.1",
    "http": "npm:jspm-nodelibs-http@0.2.0",
    "humanize-duration": "github:EvanHahn/HumanizeDuration.js@3.15.1",
    "jquery": "npm:jquery@2.2.4",
    "katex": "npm:katex@0.7.1",
    "module": "npm:jspm-nodelibs-module@0.2.1",
    "moment": "npm:moment@2.24.0",
    "moment-timezone": "npm:moment-timezone@0.5.21",
    "net": "npm:jspm-nodelibs-net@0.2.1",
    "ng-file-upload": "npm:ng-file-upload@12.2.13",
    "ngstorage": "npm:ngstorage@0.3.11",
    "os": "npm:jspm-nodelibs-os@0.2.1",
    "path": "npm:jspm-nodelibs-path@0.2.3",
    "process": "npm:jspm-nodelibs-process@0.2.1",
    "rangyinputs": "github:jigargosar/rangyinputs@1.2.0",
    "readline": "npm:jspm-nodelibs-readline@0.2.1",
    "reveal.js": "npm:reveal.js@3.8.0",
    "stream": "npm:jspm-nodelibs-stream@0.2.1",
    "string_decoder": "npm:jspm-nodelibs-string_decoder@0.2.2",
    "text": "github:systemjs/plugin-text@0.0.9",
    "ts": "github:frankwallis/plugin-typescript@9.0.0",
    "tslib": "npm:tslib@1.9.3",
    "typescript": "npm:typescript@3.5.1",
    "url": "npm:jspm-nodelibs-url@0.2.1",
    "url-search-params-polyfill": "npm:url-search-params-polyfill@7.0.0",
    "util": "npm:jspm-nodelibs-util@0.2.2",
    "vm": "npm:jspm-nodelibs-vm@0.2.1"
  },
  packages: {
    "npm:angular-eonasdan-datetimepicker@0.3.9": {
      "map": {
        "angular": "npm:angular@1.7.5"
      }
    },
    "npm:angular-timer@1.3.5": {
      "map": {}
    },
    "npm:browserify-rsa@4.0.1": {
      "map": {
        "bn.js": "npm:bn.js@4.11.8",
        "randombytes": "npm:randombytes@2.1.0"
      }
    },
    "npm:browserify-sign@4.0.4": {
      "map": {
        "bn.js": "npm:bn.js@4.11.8",
        "browserify-rsa": "npm:browserify-rsa@4.0.1",
        "create-hash": "npm:create-hash@1.2.0",
        "create-hmac": "npm:create-hmac@1.1.7",
        "elliptic": "npm:elliptic@6.4.1",
        "inherits": "npm:inherits@2.0.3",
        "parse-asn1": "npm:parse-asn1@5.1.4"
      }
    },
    "npm:buffer-xor@1.0.3": {
      "map": {}
    },
    "npm:core-util-is@1.0.2": {
      "map": {}
    },
    "npm:des.js@1.0.0": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.1"
      }
    },
    "npm:eonasdan-bootstrap-datetimepicker@4.17.47": {
      "map": {
        "jquery": "npm:jquery@2.2.4"
      }
    },
    "npm:ng-file-upload@12.2.13": {
      "map": {}
    },
    "npm:punycode@1.3.2": {
      "map": {}
    },
    "npm:jspm-nodelibs-crypto@0.2.1": {
      "map": {
        "crypto-browserify": "npm:crypto-browserify@3.12.0"
      }
    },
    "npm:jspm-nodelibs-os@0.2.1": {
      "map": {
        "os-browserify": "npm:os-browserify@0.2.1"
      }
    },
    "npm:jspm-nodelibs-http@0.2.0": {
      "map": {
        "http-browserify": "npm:stream-http@2.8.3"
      }
    },
    "npm:jspm-nodelibs-url@0.2.1": {
      "map": {
        "url": "npm:url@0.11.0"
      }
    },
    "npm:url@0.11.0": {
      "map": {
        "punycode": "npm:punycode@1.3.2",
        "querystring": "npm:querystring@0.2.0"
      }
    },
    "npm:string_decoder@0.10.31": {
      "map": {}
    },
    "npm:jspm-nodelibs-stream@0.2.1": {
      "map": {
        "stream-browserify": "npm:stream-browserify@2.0.2"
      }
    },
    "github:twbs/bootstrap@3.3.7": {
      "map": {
        "jquery": "npm:jquery@2.2.4"
      }
    },
    "npm:hmac-drbg@1.0.1": {
      "map": {
        "hash.js": "npm:hash.js@1.1.7",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.1",
        "minimalistic-crypto-utils": "npm:minimalistic-crypto-utils@1.0.1"
      }
    },
    "npm:crypto-browserify@3.12.0": {
      "map": {
        "randomfill": "npm:randomfill@1.0.4",
        "create-ecdh": "npm:create-ecdh@4.0.3",
        "browserify-sign": "npm:browserify-sign@4.0.4",
        "browserify-cipher": "npm:browserify-cipher@1.0.1",
        "public-encrypt": "npm:public-encrypt@4.0.3",
        "pbkdf2": "npm:pbkdf2@3.0.17",
        "randombytes": "npm:randombytes@2.1.0",
        "diffie-hellman": "npm:diffie-hellman@5.0.3",
        "create-hmac": "npm:create-hmac@1.1.7",
        "create-hash": "npm:create-hash@1.2.0",
        "inherits": "npm:inherits@2.0.3"
      }
    },
    "npm:evp_bytestokey@1.0.3": {
      "map": {
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "md5.js": "npm:md5.js@1.3.5"
      }
    },
    "npm:cipher-base@1.0.4": {
      "map": {
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "inherits": "npm:inherits@2.0.3"
      }
    },
    "npm:miller-rabin@4.0.1": {
      "map": {
        "bn.js": "npm:bn.js@4.11.8",
        "brorand": "npm:brorand@1.1.0"
      }
    },
    "npm:jspm-nodelibs-string_decoder@0.2.2": {
      "map": {
        "string_decoder": "npm:string_decoder@0.10.31"
      }
    },
    "npm:hash-base@3.0.4": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "safe-buffer": "npm:safe-buffer@5.1.2"
      }
    },
    "npm:jspm-nodelibs-buffer@0.2.3": {
      "map": {
        "buffer": "npm:buffer@5.2.1"
      }
    },
    "npm:diffie-hellman@5.0.3": {
      "map": {
        "randombytes": "npm:randombytes@2.1.0",
        "miller-rabin": "npm:miller-rabin@4.0.1",
        "bn.js": "npm:bn.js@4.11.8"
      }
    },
    "npm:create-hmac@1.1.7": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "create-hash": "npm:create-hash@1.2.0",
        "cipher-base": "npm:cipher-base@1.0.4",
        "ripemd160": "npm:ripemd160@2.0.2",
        "sha.js": "npm:sha.js@2.4.11",
        "safe-buffer": "npm:safe-buffer@5.1.2"
      }
    },
    "npm:randomfill@1.0.4": {
      "map": {
        "randombytes": "npm:randombytes@2.1.0",
        "safe-buffer": "npm:safe-buffer@5.1.2"
      }
    },
    "npm:create-hash@1.2.0": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "cipher-base": "npm:cipher-base@1.0.4",
        "ripemd160": "npm:ripemd160@2.0.2",
        "sha.js": "npm:sha.js@2.4.11",
        "md5.js": "npm:md5.js@1.3.5"
      }
    },
    "npm:browserify-cipher@1.0.1": {
      "map": {
        "evp_bytestokey": "npm:evp_bytestokey@1.0.3",
        "browserify-des": "npm:browserify-des@1.0.2",
        "browserify-aes": "npm:browserify-aes@1.2.0"
      }
    },
    "npm:browserify-des@1.0.2": {
      "map": {
        "cipher-base": "npm:cipher-base@1.0.4",
        "inherits": "npm:inherits@2.0.3",
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "des.js": "npm:des.js@1.0.0"
      }
    },
    "npm:ripemd160@2.0.2": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "hash-base": "npm:hash-base@3.0.4"
      }
    },
    "npm:browserify-aes@1.2.0": {
      "map": {
        "cipher-base": "npm:cipher-base@1.0.4",
        "evp_bytestokey": "npm:evp_bytestokey@1.0.3",
        "inherits": "npm:inherits@2.0.3",
        "create-hash": "npm:create-hash@1.2.0",
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "buffer-xor": "npm:buffer-xor@1.0.3"
      }
    },
    "npm:sha.js@2.4.11": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "safe-buffer": "npm:safe-buffer@5.1.2"
      }
    },
    "npm:create-ecdh@4.0.3": {
      "map": {
        "elliptic": "npm:elliptic@6.4.1",
        "bn.js": "npm:bn.js@4.11.8"
      }
    },
    "npm:elliptic@6.4.1": {
      "map": {
        "bn.js": "npm:bn.js@4.11.8",
        "inherits": "npm:inherits@2.0.3",
        "brorand": "npm:brorand@1.1.0",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.1",
        "hmac-drbg": "npm:hmac-drbg@1.0.1",
        "minimalistic-crypto-utils": "npm:minimalistic-crypto-utils@1.0.1",
        "hash.js": "npm:hash.js@1.1.7"
      }
    },
    "npm:asn1.js@4.10.1": {
      "map": {
        "bn.js": "npm:bn.js@4.11.8",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.1",
        "inherits": "npm:inherits@2.0.3"
      }
    },
    "npm:readable-stream@2.3.6": {
      "map": {
        "string_decoder": "npm:string_decoder@1.1.1",
        "inherits": "npm:inherits@2.0.3",
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "core-util-is": "npm:core-util-is@1.0.2",
        "process-nextick-args": "npm:process-nextick-args@2.0.0",
        "isarray": "npm:isarray@1.0.0",
        "util-deprecate": "npm:util-deprecate@1.0.2"
      }
    },
    "npm:string_decoder@1.1.1": {
      "map": {
        "safe-buffer": "npm:safe-buffer@5.1.2"
      }
    },
    "npm:moment-timezone@0.5.21": {
      "map": {
        "moment": "npm:moment@2.24.0"
      }
    },
    "npm:katex@0.7.1": {
      "map": {
        "match-at": "npm:match-at@0.1.1"
      }
    },
    "npm:angularjs-color-picker@3.4.8": {
      "map": {
        "angular": "npm:angular@1.7.5",
        "tinycolor2": "npm:tinycolor2@1.4.1"
      }
    },
    "npm:angular-ui-grid@4.6.3": {
      "map": {
        "angular": "npm:angular@1.7.5"
      }
    },
    "npm:angular-diff-match-patch@0.7.4": {
      "map": {
        "diff-match-patch": "npm:diff-match-patch@1.0.4",
        "angular": "npm:angular@1.7.5"
      }
    },
    "npm:buffer@5.2.1": {
      "map": {
        "base64-js": "npm:base64-js@1.3.0",
        "ieee754": "npm:ieee754@1.1.13"
      }
    },
    "npm:public-encrypt@4.0.3": {
      "map": {
        "randombytes": "npm:randombytes@2.1.0",
        "create-hash": "npm:create-hash@1.2.0",
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "browserify-rsa": "npm:browserify-rsa@4.0.1",
        "parse-asn1": "npm:parse-asn1@5.1.4",
        "bn.js": "npm:bn.js@4.11.8"
      }
    },
    "npm:pbkdf2@3.0.17": {
      "map": {
        "create-hmac": "npm:create-hmac@1.1.7",
        "create-hash": "npm:create-hash@1.2.0",
        "ripemd160": "npm:ripemd160@2.0.2",
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "sha.js": "npm:sha.js@2.4.11"
      }
    },
    "npm:randombytes@2.1.0": {
      "map": {
        "safe-buffer": "npm:safe-buffer@5.1.2"
      }
    },
    "npm:md5.js@1.3.5": {
      "map": {
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "inherits": "npm:inherits@2.0.3",
        "hash-base": "npm:hash-base@3.0.4"
      }
    },
    "npm:parse-asn1@5.1.4": {
      "map": {
        "browserify-aes": "npm:browserify-aes@1.2.0",
        "create-hash": "npm:create-hash@1.2.0",
        "evp_bytestokey": "npm:evp_bytestokey@1.0.3",
        "pbkdf2": "npm:pbkdf2@3.0.17",
        "safe-buffer": "npm:safe-buffer@5.1.2",
        "asn1.js": "npm:asn1.js@4.10.1"
      }
    },
    "npm:hash.js@1.1.7": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.1"
      }
    },
    "npm:stream-browserify@2.0.2": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "readable-stream": "npm:readable-stream@2.3.6"
      }
    },
    "npm:stream-http@2.8.3": {
      "map": {
        "inherits": "npm:inherits@2.0.3",
        "to-arraybuffer": "npm:to-arraybuffer@1.0.1",
        "builtin-status-codes": "npm:builtin-status-codes@3.0.0",
        "xtend": "npm:xtend@4.0.1",
        "readable-stream": "npm:readable-stream@2.3.6"
      }
    },
    "npm:io-ts@1.9.0": {
      "map": {
        "fp-ts": "npm:fp-ts@1.18.2"
      }
    },
    "npm:chart.js@2.8.0": {
      "map": {
        "chartjs-color": "npm:chartjs-color@2.3.0",
        "moment": "npm:moment@2.24.0"
      }
    },
    "npm:chartjs-color@2.3.0": {
      "map": {
        "color-convert": "npm:color-convert@0.5.3",
        "chartjs-color-string": "npm:chartjs-color-string@0.6.0"
      }
    },
    "npm:chartjs-color-string@0.6.0": {
      "map": {
        "color-name": "npm:color-name@1.1.4"
      }
    }
  }
});
