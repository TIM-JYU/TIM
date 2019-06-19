import express from "express";
import * as t from "io-ts";
import ivm from "isolated-vm";
import {Max1000} from "../public/javascripts/jsrunnertypes";

const router = express.Router();

const RunScriptInput = t.intersection([t.type({
    code: t.string,
    data: t.unknown,
}), t.partial({
    timeout: Max1000,
})]);

router.post("/", (req, res, next) => {
    const decoded = RunScriptInput.decode(req.body);
    if (decoded.isLeft()) {
        res.status(400);
        res.send({error: "Invalid input to jsrunner runScript route."});
        return;
    }
    const inputs = decoded.value;
    const isolate = new ivm.Isolate({
        memoryLimit: 128, // in MB
        inspector: false,
    });
    let script;
    try {
        script = isolate.compileScriptSync(
            `
            const data = JSON.parse(g);
            function runScript() {
                ${inputs.code}
            }
            JSON.stringify(runScript())`,
            {
                filename: "script.js",
            },
        );
    } catch (e) {
        res.send({error: e.message});
        return;
    }
    const ctx = isolate.createContextSync({inspector: false});
    ctx.global.setSync("g", JSON.stringify(inputs.data));
    try {
        const result = script.runSync(ctx, {timeout: inputs.timeout || 500});
        if (result === undefined) {
            res.send({error: "Script failed to return anything (the return value must be JSON serializable)."});
        } else {
            res.send({result: JSON.parse(result)});
        }
    } catch (e) {
        res.send({error: e.message});
    }
});

export default router;
