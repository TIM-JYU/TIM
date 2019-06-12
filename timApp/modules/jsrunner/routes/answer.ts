import express from "express";
import {JsrunnerAnswer} from "../servertypes";
import ivm from "isolated-vm";
import {readFileSync} from "fs";

const router = express.Router();
const toolsSource = readFileSync("./dist/tools.js").toString();

router.put("/", (req, res, next) => {
    const decoded = JsrunnerAnswer.decode(req.body);
    if (decoded.isLeft()) {
        res.send({web: {error: "Incorrect input format."}});
        return;
    }
    const value = decoded.value;
    const uAndF = value.input.data;
    const aliases = value.input.aliases;
    const regex = /[0-9]+\./;
    const currDoc = value.taskID.match(regex);
    if (!currDoc) {
        res.send({web: {error: "Taskid missing or incorrect format."}});
        return;
    }
    const program = value.markup.program;
    const markup = value.markup;

    const isolate = new ivm.Isolate({
        memoryLimit: 128, // in MB
        inspector: false,
    });

    const script = isolate.compileScriptSync(
        `
        ${toolsSource}
        const Tools = toolsjs.default;
        const d = JSON.parse(g);
        const data = d.data;
        const currDoc = d.currDoc;
        const markup = d.markup;
        const aliases = d.aliases;
        let saveUsersFields = [];
        let output = "";
        let error = "";
        for (const user of data) {
            const tools = new Tools(user, currDoc[0], markup, aliases);
            function runProgram(saveUsersFields, output, error) {
                ${program}
            }
            runProgram();
            saveUsersFields.push(tools.getResult());
            output += tools.getPrint();
            error = tools.getError();
        }
        JSON.stringify({res: saveUsersFields, pr: output, err: error})
        `,
        {
            filename: "script.js",
        }
    );
    const ctx = isolate.createContextSync({inspector: false});
    ctx.global.setSync("g", JSON.stringify({
        data: uAndF,
        currDoc,
        markup,
        aliases,
    }));
    const result = JSON.parse(script.runSync(ctx, {timeout: 1000}));
    res.send({save: result.res, web: {result: "points saved", print: result.pr, error: result.err}});
});

export default router;
