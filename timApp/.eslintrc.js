module.exports = {
    "extends": [
        ".eslintrc_tim",
        "prettier",
    ],
    // Overrides any options disabled by eslint-config-prettier
    // See https://github.com/prettier/eslint-config-prettier#special-rules
    "rules": {
        "curly": ["error", "all"]
    }
}
