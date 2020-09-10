module.exports = {
    "env": {
        "browser": true,
        "es6": true,
        "node": true
    },
    "parser": "@typescript-eslint/parser",
    "parserOptions": {
        "project": "tsconfig.json",
        "sourceType": "module"
    },
    "plugins": [
        "@typescript-eslint",
        "import",
        "prefer-arrow"
    ],
    "ignorePatterns": [
        "modules/cs/jsav/",
        "modules/cs/mindsim/",
        "modules/cs/simcir/",
        "modules/jsrunner/server/",
        "static/scripts/tim/environments/environment.prod.ts",
        "**/*.worker.ts"
    ],
    "rules": {
        "@typescript-eslint/adjacent-overload-signatures": "error",
        "@typescript-eslint/array-type": "off",
        "@typescript-eslint/ban-types": "error",
        "@typescript-eslint/naming-convention": [
            "error",
            {"selector": "class", "format": ["PascalCase"]}
        ],
        "@typescript-eslint/consistent-type-assertions": "error",
        "@typescript-eslint/consistent-type-definitions": "error",
        "@typescript-eslint/explicit-member-accessibility": [
            "off",
            {
                "accessibility": "explicit"
            }
        ],
        "@typescript-eslint/indent": "off",
        "@typescript-eslint/interface-name-prefix": "off",
        "@typescript-eslint/member-delimiter-style": [
            "off",
            {
                "multiline": {
                    "delimiter": "semi",
                    "requireLast": true
                },
                "singleline": {
                    "delimiter": "semi",
                    "requireLast": false
                }
            }
        ],
        "@typescript-eslint/member-ordering": "off",
        "@typescript-eslint/no-empty-function": "off",
        "@typescript-eslint/no-empty-interface": "off",
        "@typescript-eslint/no-explicit-any": "error",
        "@typescript-eslint/no-for-in-array": "error",
        "@typescript-eslint/no-misused-new": "error",
        "@typescript-eslint/no-namespace": "error",
        "@typescript-eslint/no-parameter-properties": "off",
        "@typescript-eslint/no-unnecessary-type-assertion": "error",
        "@typescript-eslint/no-unnecessary-condition": "off", // TODO enable
        "@typescript-eslint/no-unnecessary-boolean-literal-compare": "error",
        "@typescript-eslint/no-extra-non-null-assertion": "error",
        "@typescript-eslint/no-use-before-declare": "off",
        "@typescript-eslint/no-var-requires": "error",
        "@typescript-eslint/prefer-for-of": "error",
        "@typescript-eslint/prefer-function-type": "error",
        "@typescript-eslint/prefer-namespace-keyword": "error",
        "@typescript-eslint/no-floating-promises": "off", // TODO enable
        "@typescript-eslint/require-await": "error",
        "quotes": "off",
        "@typescript-eslint/quotes": [
            "error",
            "double",
            {
                "avoidEscape": true,
                "allowTemplateLiterals": true
            }
        ],
        "@typescript-eslint/restrict-plus-operands": "off", // TODO maybe enable
        "@typescript-eslint/semi": [
            "error",
            "always"
        ],
        "@typescript-eslint/strict-boolean-expressions": "off", // TODO enable
        "@typescript-eslint/restrict-template-expressions": "error",
        "@typescript-eslint/no-base-to-string": "error",
        "@typescript-eslint/triple-slash-reference": "error",
        "@typescript-eslint/type-annotation-spacing": "error",
        "@typescript-eslint/unbound-method": "off",
        "@typescript-eslint/unified-signatures": "error",
        "@typescript-eslint/prefer-includes": "error",
        "@typescript-eslint/prefer-nullish-coalescing": "error",
        "@typescript-eslint/prefer-optional-chain": "error",
        "@typescript-eslint/prefer-string-starts-ends-with": "error",
        "@typescript-eslint/prefer-as-const": "error",
        "@typescript-eslint/switch-exhaustiveness-check": "error",
        "@typescript-eslint/no-shadow": "error",
        "@typescript-eslint/no-unsafe-member-access": "error",
        "@typescript-eslint/no-unused-vars": [
            "error", {
                "vars": "all",
                "args": "none",
                "ignoreRestSiblings": false,
                "varsIgnorePattern": "^(.*[iI]gnored|_)$",
            }],
        "@typescript-eslint/ban-ts-comment": [
            "error",
            {
                "ts-ignore": true,
                "ts-expect-error": false,
                minimumDescriptionLength: 0,
            }
        ],
        "arrow-body-style": "off",
        "arrow-parens": [
            "error",
            "always"
        ],
        "camelcase": "off",
        "capitalized-comments": "off",
        "comma-dangle": [
            "error",
            "always-multiline"
        ],
        "complexity": "off",
        "constructor-super": "error",
        "curly": "error",
        "dot-notation": "error",
        "eol-last": "error",
        "eqeqeq": [
            "off",
            "always"
        ],
        "guard-for-in": "error",
        "id-blacklist": [
            "error",
            "any",
            "Number",
            "number",
            "string",
            "Boolean",
            "boolean",
            "Undefined"
        ],
        "id-match": "error",
        "import/order": "error",
        "max-classes-per-file": "off",
        "max-len": "off",
        "new-parens": "error",
        "brace-style": "error",
        "no-bitwise": "error",
        "no-caller": "error",
        "no-cond-assign": "error",
        "no-console": "off",
        "no-debugger": "error",
        "no-empty": "off",
        "no-eval": "error",
        "no-extra-bind": "error",
        "no-fallthrough": "off",
        "no-invalid-this": "off",
        "no-multiple-empty-lines": "error",
        "no-new-wrappers": "error",
        "no-throw-literal": "error",
        "no-trailing-spaces": "error",
        "no-undef-init": "error",
        "no-underscore-dangle": "error",
        "no-unsafe-finally": "error",
        "no-unsafe-negation": "error",
        "no-unused-expressions": "error",
        "valid-jsdoc": "off", // TODO enable
        "no-unused-labels": "error",
        "no-var": "error",
        "object-shorthand": "off",
        "one-var": [
            "error",
            "never"
        ],
        "prefer-arrow/prefer-arrow-functions": "off",
        "prefer-const": "error",
        "quote-props": [
            "error",
            "consistent-as-needed"
        ],
        "radix": "error",
        "space-before-function-paren": [
            "error",
            {
                "anonymous": "never",
                "asyncArrow": "always",
                "named": "never"
            }
        ],
        "spaced-comment": "error",
        "space-in-parens": ["error", "never"],
        "use-isnan": "error",
        "valid-typeof": "off",
    }
};
