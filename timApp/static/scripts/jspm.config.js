SystemJS.config({
  paths: {
    "github:": "jspm_packages/github/",
    "npm:": "jspm_packages/npm/",
    "reveal": "reveal/js/reveal.js",
    "head": "reveal/lib/js/head.min.js",
    "simcir": "/cs/simcir/simcir.js",
    "simcir/basicset": "/cs/simcir/simcir-basicset.js",
    "simcir/library": "/cs/simcir/simcir-library.js",
    "simcir/oma-kirjasto": "/cs/simcir/simcir-oma-kirjasto.js",
    "mathjax": "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_SVG",
    "tim/": "tim/"
  },
  browserConfig: {
    "baseURL": "/static/scripts"
  },
  devConfig: {
    "map": {
      "core-js": "npm:core-js@1.2.7",
      "ts-runtime": "npm:babel-runtime@5.8.38",
      "plugin-typescript": "github:frankwallis/plugin-typescript@7.0.6"
    },
    "packages": {
      "npm:babel-runtime@5.8.38": {
        "map": {}
      },
      "npm:core-js@1.2.7": {
        "map": {}
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
    "tsconfig": true
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
    }
  },
  meta: {
    "npm:angular-eonasdan-datetimepicker@0.3.9/dist/angular-eonasdan-datetimepicker.js": {
      "deps": [
        "angular",
        "eonasdan-bootstrap-datetimepicker"
      ],
      "globals": {
         "moment": "npm:moment@2.18.1/moment.js"
      }
    },
    "npm:angular@1.5.11/angular.js": {
      "deps": [
        "jquery"
      ]
    },
    "npm:katex@0.7.1/dist/contrib/auto-render.min.js": {
      "globals": {
        "katex": "npm:katex@0.7.1/katex.js"
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
    "tim/show_slide_vars.ts": {
      "build": false
    },
    "tim/angularmodules.ts": {
      "build": false
    },
    "tim/plugins.ts": {
      "build": false
    },
    "tim/extramodules.ts": {
      "build": false
    },
    "tim/session.ts": {
      "build": false
    },
    "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS_SVG": {
      "format": "global",
      "exports": "MathJax"
    }
  },
  map: {
    "katex-auto-render": "npm:katex@0.7.1/dist/contrib/auto-render.min"
  },
  bundles: {
    "build/ace.js": [
      "tim/ace.ts",
      "github:ajaxorg/ace-builds@1.2.6/ext-language_tools.js"
    ],
    "build/slide.js": [
      "tim/slide.ts"
    ],
    "build/imagex.js": [
      "tim/imagex.ts"
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
    "@types/systemjs": "npm:@types/systemjs@0.20.2",
    "ace": "github:ajaxorg/ace-builds@1.2.6",
    "constants": "npm:jspm-nodelibs-constants@0.2.1",
    "events": "npm:jspm-nodelibs-events@0.2.2",
    "@types/ace": "npm:@types/ace@0.0.32",
    "@types/angular": "npm:@types/angular@1.5.23",
    "@types/angular-animate": "npm:@types/angular-animate@1.5.6",
    "@types/angular-sanitize": "npm:@types/angular-sanitize@1.3.4",
    "@types/angular-ui-bootstrap": "npm:@types/angular-ui-bootstrap@0.13.41",
    "@types/bootstrap": "npm:@types/bootstrap@3.3.32",
    "@types/chart.js": "npm:@types/chart.js@2.4.2",
    "@types/eonasdan-bootstrap-datetimepicker": "npm:@types/eonasdan-bootstrap-datetimepicker@4.17.22",
    "@types/jquery": "npm:@types/jquery@2.0.41",
    "@types/katex": "npm:@types/katex@0.5.0",
    "@types/mathjax": "npm:@types/mathjax@0.0.31",
    "@types/ngstorage": "npm:@types/ngstorage@0.3.34",
    "@types/oclazyload": "npm:@types/oclazyload@0.0.31",
    "@types/rangyinputs": "npm:@types/rangyinputs@1.2.0",
    "@types/ui-grid": "npm:@types/ui-grid@0.0.35",
    "angular": "npm:angular@1.5.11",
    "angular-animate": "npm:angular-animate@1.5.11",
    "angular-bootstrap-colorpicker": "npm:angular-bootstrap-colorpicker@3.0.26",
    "angular-eonasdan-datetimepicker": "npm:angular-eonasdan-datetimepicker@0.3.9",
    "angular-messages": "npm:angular-messages@1.5.11",
    "angular-sanitize": "npm:angular-sanitize@1.5.11",
    "angular-timer": "npm:angular-timer@1.3.5",
    "angular-touch": "npm:angular-touch@1.5.11",
    "angular-ui-bootstrap": "npm:angular-ui-bootstrap@2.5.0",
    "angular-ui-grid": "npm:angular-ui-grid@4.0.1",
    "assert": "npm:jspm-nodelibs-assert@0.2.1",
    "bootstrap": "github:twbs/bootstrap@3.3.7",
    "bootstrap-sass": "github:twbs/bootstrap-sass@3.3.7",
    "buffer": "npm:jspm-nodelibs-buffer@0.2.2",
    "chart.js": "npm:chart.js@2.5.0",
    "child_process": "npm:jspm-nodelibs-child_process@0.2.1",
    "crypto": "npm:jspm-nodelibs-crypto@0.2.1",
    "eonasdan-bootstrap-datetimepicker": "npm:eonasdan-bootstrap-datetimepicker@4.17.47",
    "fs": "npm:jspm-nodelibs-fs@0.2.1",
    "http": "npm:jspm-nodelibs-http@0.2.0",
    "humanize-duration": "npm:humanize-duration@3.10.0",
    "jquery": "npm:jquery@2.2.4",
    "katex": "npm:katex@0.7.1",
    "module": "npm:jspm-nodelibs-module@0.2.1",
    "moment": "npm:moment@2.18.1",
    "moment-timezone": "npm:moment-timezone@0.4.1",
    "net": "npm:jspm-nodelibs-net@0.2.1",
    "ng-file-upload": "npm:ng-file-upload@12.2.13",
    "ngstorage": "npm:ngstorage@0.3.11",
    "oclazyload": "npm:oclazyload@1.1.0",
    "os": "npm:jspm-nodelibs-os@0.2.1",
    "path": "npm:jspm-nodelibs-path@0.2.3",
    "process": "npm:jspm-nodelibs-process@0.2.1",
    "rangyinputs": "github:jigargosar/rangyinputs@1.2.0",
    "stream": "npm:jspm-nodelibs-stream@0.2.1",
    "string_decoder": "npm:jspm-nodelibs-string_decoder@0.2.1",
    "text": "github:systemjs/plugin-text@0.0.9",
    "ts": "github:frankwallis/plugin-typescript@7.0.6",
    "typescript": "npm:typescript@2.2.2",
    "url": "npm:jspm-nodelibs-url@0.2.1",
    "util": "npm:jspm-nodelibs-util@0.2.2",
    "vm": "npm:jspm-nodelibs-vm@0.2.1"
  },
  packages: {
    "npm:@types/angular-animate@1.5.6": {
      "map": {
        "@types/angular": "npm:@types/angular@1.5.23",
        "@types/jquery": "npm:@types/jquery@2.0.41"
      }
    },
    "npm:@types/angular-sanitize@1.3.4": {
      "map": {
        "@types/angular": "npm:@types/angular@1.5.23"
      }
    },
    "npm:@types/angular-ui-bootstrap@0.13.41": {
      "map": {
        "@types/angular": "npm:@types/angular@1.5.23"
      }
    },
    "npm:@types/angular@1.5.23": {
      "map": {
        "@types/jquery": "npm:@types/jquery@2.0.41"
      }
    },
    "npm:@types/bootstrap@3.3.32": {
      "map": {
        "@types/jquery": "npm:@types/jquery@2.0.41"
      }
    },
    "npm:@types/chart.js@2.4.2": {
      "map": {
        "@types/jquery": "npm:@types/jquery@2.0.41"
      }
    },
    "npm:@types/eonasdan-bootstrap-datetimepicker@4.17.22": {
      "map": {
        "@types/jquery": "npm:@types/jquery@2.0.41",
        "moment": "npm:moment@2.18.1"
      }
    },
    "npm:@types/oclazyload@0.0.31": {
      "map": {
        "@types/angular": "npm:@types/angular@1.5.23"
      }
    },
    "npm:@types/rangyinputs@1.2.0": {
      "map": {
        "@types/jquery": "npm:@types/jquery@2.0.41"
      }
    },
    "npm:@types/ui-grid@0.0.35": {
      "map": {
        "@types/angular": "npm:@types/angular@1.5.23",
        "@types/jquery": "npm:@types/jquery@2.0.41"
      }
    },
    "npm:angular-eonasdan-datetimepicker@0.3.9": {
      "map": {
        "angular": "npm:angular@1.5.11"
      }
    },
    "npm:angular-timer@1.3.5": {
      "map": {}
    },
    "npm:angular-ui-grid@4.0.1": {
      "map": {
        "angular": "npm:angular@1.5.11"
      }
    },
    "npm:asn1.js@4.9.1": {
      "map": {
        "bn.js": "npm:bn.js@4.11.6",
        "inherits": "npm:inherits@2.0.1",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.0"
      }
    },
    "npm:bn.js@4.11.6": {
      "map": {}
    },
    "npm:browserify-aes@1.0.6": {
      "map": {
        "buffer-xor": "npm:buffer-xor@1.0.3",
        "cipher-base": "npm:cipher-base@1.0.3",
        "create-hash": "npm:create-hash@1.1.2",
        "evp_bytestokey": "npm:evp_bytestokey@1.0.0",
        "inherits": "npm:inherits@2.0.1"
      }
    },
    "npm:browserify-cipher@1.0.0": {
      "map": {
        "browserify-aes": "npm:browserify-aes@1.0.6",
        "browserify-des": "npm:browserify-des@1.0.0",
        "evp_bytestokey": "npm:evp_bytestokey@1.0.0"
      }
    },
    "npm:browserify-des@1.0.0": {
      "map": {
        "cipher-base": "npm:cipher-base@1.0.3",
        "des.js": "npm:des.js@1.0.0",
        "inherits": "npm:inherits@2.0.1"
      }
    },
    "npm:browserify-rsa@4.0.1": {
      "map": {
        "bn.js": "npm:bn.js@4.11.6",
        "randombytes": "npm:randombytes@2.0.3"
      }
    },
    "npm:browserify-sign@4.0.4": {
      "map": {
        "bn.js": "npm:bn.js@4.11.6",
        "browserify-rsa": "npm:browserify-rsa@4.0.1",
        "create-hash": "npm:create-hash@1.1.2",
        "create-hmac": "npm:create-hmac@1.1.4",
        "elliptic": "npm:elliptic@6.4.0",
        "inherits": "npm:inherits@2.0.1",
        "parse-asn1": "npm:parse-asn1@5.1.0"
      }
    },
    "npm:buffer-xor@1.0.3": {
      "map": {}
    },
    "npm:chart.js@2.5.0": {
      "map": {
        "chartjs-color": "npm:chartjs-color@2.1.0",
        "moment": "npm:moment@2.18.1"
      }
    },
    "npm:chartjs-color-string@0.4.0": {
      "map": {
        "color-name": "npm:color-name@1.1.2"
      }
    },
    "npm:chartjs-color@2.1.0": {
      "map": {
        "chartjs-color-string": "npm:chartjs-color-string@0.4.0",
        "color-convert": "npm:color-convert@0.5.3"
      }
    },
    "npm:cipher-base@1.0.3": {
      "map": {
        "inherits": "npm:inherits@2.0.1"
      }
    },
    "npm:core-util-is@1.0.2": {
      "map": {}
    },
    "npm:create-ecdh@4.0.0": {
      "map": {
        "bn.js": "npm:bn.js@4.11.6",
        "elliptic": "npm:elliptic@6.4.0"
      }
    },
    "npm:create-hash@1.1.2": {
      "map": {
        "cipher-base": "npm:cipher-base@1.0.3",
        "inherits": "npm:inherits@2.0.1",
        "ripemd160": "npm:ripemd160@1.0.1",
        "sha.js": "npm:sha.js@2.4.8"
      }
    },
    "npm:create-hmac@1.1.4": {
      "map": {
        "create-hash": "npm:create-hash@1.1.2",
        "inherits": "npm:inherits@2.0.1"
      }
    },
    "npm:crypto-browserify@3.11.0": {
      "map": {
        "browserify-cipher": "npm:browserify-cipher@1.0.0",
        "browserify-sign": "npm:browserify-sign@4.0.4",
        "create-ecdh": "npm:create-ecdh@4.0.0",
        "create-hash": "npm:create-hash@1.1.2",
        "create-hmac": "npm:create-hmac@1.1.4",
        "diffie-hellman": "npm:diffie-hellman@5.0.2",
        "inherits": "npm:inherits@2.0.1",
        "pbkdf2": "npm:pbkdf2@3.0.9",
        "public-encrypt": "npm:public-encrypt@4.0.0",
        "randombytes": "npm:randombytes@2.0.3"
      }
    },
    "npm:des.js@1.0.0": {
      "map": {
        "inherits": "npm:inherits@2.0.1",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.0"
      }
    },
    "npm:diffie-hellman@5.0.2": {
      "map": {
        "bn.js": "npm:bn.js@4.11.6",
        "miller-rabin": "npm:miller-rabin@4.0.0",
        "randombytes": "npm:randombytes@2.0.3"
      }
    },
    "npm:elliptic@6.4.0": {
      "map": {
        "bn.js": "npm:bn.js@4.11.6",
        "brorand": "npm:brorand@1.1.0",
        "hash.js": "npm:hash.js@1.0.3",
        "hmac-drbg": "npm:hmac-drbg@1.0.0",
        "inherits": "npm:inherits@2.0.1",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.0",
        "minimalistic-crypto-utils": "npm:minimalistic-crypto-utils@1.0.1"
      }
    },
    "npm:eonasdan-bootstrap-datetimepicker@4.17.47": {
      "map": {
        "jquery": "npm:jquery@2.2.4"
      }
    },
    "npm:evp_bytestokey@1.0.0": {
      "map": {
        "create-hash": "npm:create-hash@1.1.2"
      }
    },
    "npm:hash.js@1.0.3": {
      "map": {
        "inherits": "npm:inherits@2.0.1"
      }
    },
    "npm:hmac-drbg@1.0.0": {
      "map": {
        "hash.js": "npm:hash.js@1.0.3",
        "minimalistic-assert": "npm:minimalistic-assert@1.0.0",
        "minimalistic-crypto-utils": "npm:minimalistic-crypto-utils@1.0.1"
      }
    },
    "npm:inherits@2.0.1": {
      "map": {}
    },
    "npm:katex@0.7.1": {
      "map": {
        "match-at": "npm:match-at@0.1.0"
      }
    },
    "npm:miller-rabin@4.0.0": {
      "map": {
        "bn.js": "npm:bn.js@4.11.6",
        "brorand": "npm:brorand@1.1.0"
      }
    },
    "npm:moment-timezone@0.4.1": {
      "map": {
        "moment": "npm:moment@2.6.0"
      }
    },
    "npm:ng-file-upload@12.2.13": {
      "map": {}
    },
    "npm:oclazyload@1.1.0": {
      "map": {}
    },
    "npm:parse-asn1@5.1.0": {
      "map": {
        "asn1.js": "npm:asn1.js@4.9.1",
        "browserify-aes": "npm:browserify-aes@1.0.6",
        "create-hash": "npm:create-hash@1.1.2",
        "evp_bytestokey": "npm:evp_bytestokey@1.0.0",
        "pbkdf2": "npm:pbkdf2@3.0.9"
      }
    },
    "npm:pbkdf2@3.0.9": {
      "map": {
        "create-hmac": "npm:create-hmac@1.1.4"
      }
    },
    "npm:public-encrypt@4.0.0": {
      "map": {
        "bn.js": "npm:bn.js@4.11.6",
        "browserify-rsa": "npm:browserify-rsa@4.0.1",
        "create-hash": "npm:create-hash@1.1.2",
        "parse-asn1": "npm:parse-asn1@5.1.0",
        "randombytes": "npm:randombytes@2.0.3"
      }
    },
    "npm:punycode@1.3.2": {
      "map": {}
    },
    "npm:randombytes@2.0.3": {
      "map": {}
    },
    "npm:ripemd160@1.0.1": {
      "map": {}
    },
    "npm:sha.js@2.4.8": {
      "map": {
        "inherits": "npm:inherits@2.0.1"
      }
    },
    "npm:source-map-support@0.4.14": {
      "map": {
        "source-map": "npm:source-map@0.5.6"
      }
    },
    "npm:source-map@0.5.6": {
      "map": {}
    },
    "npm:typescript@2.2.2": {
      "map": {
        "source-map-support": "npm:source-map-support@0.4.14"
      }
    },
    "npm:jspm-nodelibs-buffer@0.2.2": {
      "map": {
        "buffer": "npm:buffer@4.9.1"
      }
    },
    "npm:buffer@4.9.1": {
      "map": {
        "ieee754": "npm:ieee754@1.1.8",
        "isarray": "npm:isarray@1.0.0",
        "base64-js": "npm:base64-js@1.2.0"
      }
    },
    "npm:jspm-nodelibs-crypto@0.2.1": {
      "map": {
        "crypto-browserify": "npm:crypto-browserify@3.11.0"
      }
    },
    "npm:jspm-nodelibs-os@0.2.1": {
      "map": {
        "os-browserify": "npm:os-browserify@0.2.1"
      }
    },
    "npm:readable-stream@2.2.9": {
      "map": {
        "isarray": "npm:isarray@1.0.0",
        "inherits": "npm:inherits@2.0.1",
        "core-util-is": "npm:core-util-is@1.0.2",
        "buffer-shims": "npm:buffer-shims@1.0.0",
        "process-nextick-args": "npm:process-nextick-args@1.0.7",
        "util-deprecate": "npm:util-deprecate@1.0.2",
        "string_decoder": "npm:string_decoder@1.0.0"
      }
    },
    "npm:string_decoder@1.0.0": {
      "map": {
        "buffer-shims": "npm:buffer-shims@1.0.0"
      }
    },
    "npm:jspm-nodelibs-http@0.2.0": {
      "map": {
        "http-browserify": "npm:stream-http@2.7.0"
      }
    },
    "npm:stream-http@2.7.0": {
      "map": {
        "inherits": "npm:inherits@2.0.1",
        "readable-stream": "npm:readable-stream@2.2.9",
        "builtin-status-codes": "npm:builtin-status-codes@3.0.0",
        "to-arraybuffer": "npm:to-arraybuffer@1.0.1",
        "xtend": "npm:xtend@4.0.1"
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
    "npm:jspm-nodelibs-string_decoder@0.2.1": {
      "map": {
        "string_decoder": "npm:string_decoder@0.10.31"
      }
    },
    "npm:jspm-nodelibs-stream@0.2.1": {
      "map": {
        "stream-browserify": "npm:stream-browserify@2.0.1"
      }
    },
    "npm:stream-browserify@2.0.1": {
      "map": {
        "inherits": "npm:inherits@2.0.1",
        "readable-stream": "npm:readable-stream@2.2.9"
      }
    },
    "github:twbs/bootstrap@3.3.7": {
      "map": {
        "jquery": "npm:jquery@2.2.4"
      }
    }
  }
});
