import express from "express";
import * as t from "io-ts";
import ivm from "isolated-vm";
import {isLeft} from "fp-ts/lib/Either";
import {Max1000} from "../../shared/jsrunnertypes";
console.log("rs");
const router = express.Router();

const RunScriptInput = t.intersection([
    t.type({
        code: t.string,
        data: t.unknown,
    }),
    t.partial({
        timeout: Max1000,
    }),
]);

function numberLines(s: string, delta: number): string {
    const lines = s.split("\n");
    let result = "";
    for (let i = 0; i < lines.length; i++) {
        const space = i + delta < 10 ? "0" : "";
        result += space + (i + delta) + ": " + lines[i] + "\n";
    }
    return result;
}

router.post("/", async (req, res, next) => {
    const decoded = RunScriptInput.decode(req.body);
    if (isLeft(decoded)) {
        res.status(400);
        res.json({error: "Invalid input to jsrunner runScript route."});
        return;
    }
    const inputs = decoded.right;
    const isolate = new ivm.Isolate({
        memoryLimit: 128, // in MB
        inspector: false,
    });
    let script;
    try {
        script = await isolate.compileScript(
            `
const data = JSON.parse(g);
let output = "";


function print(s) { output += s; }

function println(s) { print(s + "\\n"); }
// Empty lines so we get user code to row 11
function runMyScript() {
${inputs.code}
}

function runScript() {
    let result = runMyScript();
    return { output: output, result: result };
}
JSON.stringify(runScript())`,
            {
                filename: "script.js",
            }
        );
    } catch (e) {
        const lines = "\n<pre>\n" + numberLines(inputs.code, 11) + "</pre>\n";
        const err = e as Error;
        res.json({error: err.message + lines});
        return;
    }
    const ctx = await isolate.createContext({inspector: false});
    await ctx.global.set("g", JSON.stringify(inputs.data));
    try {
        const result = (await script.run(ctx, {
            timeout: inputs.timeout ?? 500,
        })) as string;
        const jresult = JSON.parse(result) as {
            result: unknown;
            output: unknown;
        };
        if ("result" in jresult) {
            res.json({result: jresult.result, output: jresult.output});
        } else {
            res.json({
                error:
                    "Script failed to return anything (the return value must be JSON serializable).",
            });
        }
    } catch (e) {
        const err = e as Error;
        res.json({error: err.message});
    }
});

export default router;
