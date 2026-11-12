// Shared ESLint rules of TIM (flat config).
//
// The JSRunner server (modules/angjsrunner/server) lints itself with its own TypeScript version.
// Rules that use type information silently stop working if the parser and the typescript-eslint plugin
// do not share the same TypeScript, so those two can be passed in by the caller.
const globals = require("globals");
const prettierConfig = require("eslint-config-prettier");
const importPlugin = require("eslint-plugin-import");
const importAliasPlugin = require("@limegrass/eslint-plugin-import-alias");

module.exports = function createTimConfig({
    tsconfigRootDir,
    parser = require("@typescript-eslint/parser"),
    tsPlugin = require("@typescript-eslint/eslint-plugin"),
    ignores = [],
}) {
    return [
        {
            ignores,
        },
        {
            files: ["**/*.ts"],
            languageOptions: {
                parser,
                parserOptions: {
                    project: "tsconfig.json",
                    tsconfigRootDir,
                    sourceType: "module",
                },
                globals: {
                    ...globals.browser,
                    ...globals.es2015,
                    ...globals.node,
                },
            },
            plugins: {
                "@typescript-eslint": tsPlugin,
                "import": importPlugin,
                "@limegrass/import-alias": importAliasPlugin,
            },
            rules: {
                "@limegrass/import-alias/import-alias": "error",
                "@typescript-eslint/adjacent-overload-signatures": "error",
                "@typescript-eslint/array-type": "off",
                // These three replace the former @typescript-eslint/ban-types rule.
                "@typescript-eslint/no-unsafe-function-type": "error",
                "@typescript-eslint/no-wrapper-object-types": "error",
                "@typescript-eslint/no-empty-object-type": [
                    "error",
                    {allowInterfaces: "always"},
                ],
                "@typescript-eslint/naming-convention": [
                    "error",
                    {selector: "class", format: ["PascalCase"]},
                ],
                "@typescript-eslint/consistent-type-assertions": "error",
                // This is intentionally off. See https://github.com/Microsoft/TypeScript/issues/15300
                "@typescript-eslint/consistent-type-definitions": "off",
                "@typescript-eslint/consistent-type-imports": "error",
                "@typescript-eslint/explicit-member-accessibility": [
                    "off",
                    {
                        accessibility: "explicit",
                    },
                ],
                "@typescript-eslint/member-ordering": "off",
                "@typescript-eslint/no-empty-function": "off",
                "@typescript-eslint/no-empty-interface": "off",
                "@typescript-eslint/no-explicit-any": "error",
                "@typescript-eslint/no-for-in-array": "error",
                "@typescript-eslint/no-misused-new": "error",
                "@typescript-eslint/no-namespace": "error",
                "@typescript-eslint/parameter-properties": "off",
                "@typescript-eslint/no-unnecessary-type-assertion": "error",
                "@typescript-eslint/no-unnecessary-condition": "off", // TODO enable
                "@typescript-eslint/no-unnecessary-boolean-literal-compare":
                    "error",
                "@typescript-eslint/no-extra-non-null-assertion": "error",
                "@typescript-eslint/no-require-imports": "error",
                "@typescript-eslint/prefer-for-of": "error",
                "@typescript-eslint/prefer-function-type": "error",
                "@typescript-eslint/prefer-namespace-keyword": "error",
                "@typescript-eslint/no-floating-promises": "off", // TODO enable
                "@typescript-eslint/require-await": "error",
                "@typescript-eslint/restrict-plus-operands": "off", // TODO maybe enable
                "@typescript-eslint/strict-boolean-expressions": "off", // TODO enable
                "@typescript-eslint/restrict-template-expressions": "error",
                "@typescript-eslint/no-base-to-string": "error",
                "@typescript-eslint/triple-slash-reference": "error",
                "@typescript-eslint/unbound-method": "off",
                "@typescript-eslint/unified-signatures": "error",
                "@typescript-eslint/prefer-includes": "error",
                // The options keep the checks that typescript-eslint 5 had; the newer ones
                // (ternaries, "if (!x) x = y") would change behavior for falsy non-nullish values.
                "@typescript-eslint/prefer-nullish-coalescing": [
                    "error",
                    {
                        ignoreConditionalTests: true,
                        ignoreIfStatements: true,
                        ignoreMixedLogicalExpressions: true,
                        ignoreTernaryTests: true,
                    },
                ],
                "@typescript-eslint/prefer-optional-chain": "error",
                "@typescript-eslint/prefer-string-starts-ends-with": "error",
                "@typescript-eslint/prefer-as-const": "error",
                "@typescript-eslint/switch-exhaustiveness-check": [
                    "error",
                    {considerDefaultExhaustiveForUnions: true},
                ],
                "@typescript-eslint/no-shadow": "error",
                "@typescript-eslint/no-unsafe-member-access": "error",
                "@typescript-eslint/no-unused-vars": [
                    "error",
                    {
                        vars: "all",
                        args: "none",
                        caughtErrors: "none",
                        ignoreRestSiblings: false,
                        varsIgnorePattern: "^(.*[iI]gnored|_)$",
                    },
                ],
                "@typescript-eslint/ban-ts-comment": [
                    "error",
                    {
                        "ts-ignore": true,
                        "ts-expect-error": false,
                        "minimumDescriptionLength": 0,
                    },
                ],
                "@typescript-eslint/consistent-indexed-object-style": "error",
                "@typescript-eslint/no-confusing-void-expression": [
                    "off",
                    {ignoreArrowShorthand: true},
                ], // TODO enable
                "@typescript-eslint/non-nullable-type-assertion-style": "off", // TODO enable (buggy right now)
                "camelcase": "off",
                "capitalized-comments": "off",
                "complexity": "off",
                "constructor-super": "error",
                "dot-notation": "error",
                "eqeqeq": ["off", "always"],
                "guard-for-in": "error",
                "id-denylist": [
                    "error",
                    "any",
                    "Number",
                    "number",
                    "string",
                    "Boolean",
                    "boolean",
                    "Undefined",
                ],
                "id-match": "error",
                "import/order": "error",
                "max-classes-per-file": "off",
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
                "no-new-wrappers": "error",
                "no-throw-literal": "error",
                "no-undef-init": "error",
                "no-underscore-dangle": "error",
                "no-unsafe-finally": "error",
                "no-unsafe-negation": "error",
                "no-unused-expressions": "error",
                "no-unused-labels": "error",
                "no-var": "error",
                "object-shorthand": "off",
                "one-var": ["error", "never"],
                "prefer-const": "error",
                "radix": "error",
                "spaced-comment": "error",
                "use-isnan": "error",
                "valid-typeof": "off",
            },
        },
        // Formatting is left to Prettier.
        prettierConfig,
        {
            files: ["**/*.ts"],
            // Overrides any options disabled by eslint-config-prettier
            // See https://github.com/prettier/eslint-config-prettier#special-rules
            rules: {
                curly: ["error", "all"],
            },
        },
    ];
};
