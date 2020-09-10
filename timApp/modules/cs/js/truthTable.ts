import {evaluate} from "mathjs";

const replacements = [
    ["v", "|"],
    ["^", "&"],
    ["∼", "not "],
    ["~", "not "],
    ["∧", "&"],
    ["∨", "|"],
    ["xor", "^|"],
    ["and", "&"],
    ["or", "|"],
    ["ja", "&"],
    ["ei", "not"],
    ["tai", "|"],
];

// The letters n, o, t are not allowed because Math.js logical not is "not".
const allowedVariables = "abcdefghijklmpqrsuvwxy";

export function truthTable(sentence: string) {
    let input = sentence.toLowerCase();
    for (const [froms, tos] of replacements) {
        input = input.split(froms).join(tos);
    }

    let header = "";
    let count = 0;
    let rowsInTable = 1;
    for (const c of allowedVariables) {
        if (input.includes(c)) {
            header += c + " ";
            const zv = `z[${count + 1}]`; // Math.js has 1-based indexes
            input = input.split(c).join(zv);
            count++;
            rowsInTable *= 2;
        }
    }

    const sents = sentence.split(";").map((s) => s.trim());
    const fills = [];
    for (let i = 0; i < sents.length; i++) {
        fills[i] = " ".repeat(sents[i].length);
    }
    header += "  " + sents.join("  ");
    const line = "-".repeat(header.length);
    let result = `${header}\n${line}\n`;
    const inp = input.split(";");
    for (let n = 0; n < rowsInTable; n++) {
        const z = [];
        for (let i = 0; i < count; i++) {
            // eslint-disable-next-line no-bitwise
            z.push((n >> (count - 1 - i)) & 1);
        }
        result +=
            z.reduce((str, binaryValue) => str + binaryValue + " ", "") + "= ";
        for (let i = 0; i < inp.length; i++) {
            try {
                result += " " + (evaluate(inp[i], {z}) ? 1 : 0) + fills[i];
            } catch (e) {
                return result + "\n" + e + "\n";
            }
        }
        result += "\n";
    }
    return result;
}
