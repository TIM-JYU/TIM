import {parse} from "acorn";

function compileProgram(code: string): string {
    // const ret = child_process.spawnSync("acorn", ["--silent"], {input: code});
    const ret = parse(code);
    return ret.toString();
}

console.log("1: " + compileProgram("pri int('kana);"));
console.log("2: " + compileProgram("print('kana);"));
console.log("3: " + compileProgram("print('kana');"));
