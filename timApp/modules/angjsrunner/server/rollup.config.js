import commonjs from "rollup-plugin-commonjs";
import resolve from "rollup-plugin-node-resolve";
import typescript from "rollup-plugin-typescript";

// noinspection JSUnusedGlobalSymbols
export default {
    input: "./routes/tools.ts",
    output: {
        file: "./dist/tools.js",
        format: "iife",
        name: "toolsjs",
    },
    plugins: [
        commonjs(),
        resolve(),
        typescript({
            include: [
                "*.ts",
                "**/*.ts",
                "../../../static/scripts/tim/**/*.ts",
            ],
        }),
    ],
    onwarn(warning) {
        // Disable useless warning.
        if (warning.code === "THIS_IS_UNDEFINED") {
            return;
        }
        console.log(warning.message);
    },
};
