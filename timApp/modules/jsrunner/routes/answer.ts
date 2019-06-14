import express from "express";
import {AliasDataT, JsrunnerAnswer, UserFieldDataT} from "../servertypes";
import ivm from "isolated-vm";
import {readFileSync} from "fs";
import Tools from "./tools";
import {IJsRunnerMarkup} from "../public/javascripts/jsrunnertypes";

const router = express.Router();

// Every time tools.ts (or some of its dependencies) is modified, dist/tools.js must be
// rebuilt with 'npm run buildtools'.
const toolsSource = readFileSync("./dist/tools.js").toString();

interface IRunnerData {
    aliases: AliasDataT;
    currDoc: string;
    data: UserFieldDataT[];
    markup: IJsRunnerMarkup;
    program: string;
}

/**
 * Runs jsrunner script with the provided data and returns the result.
 *
 * This function must NEVER be called outside isolate-vm!
 *
 * @param d The data to use.
 */
function runner(d: IRunnerData) {
    const data = d.data;
    const currDoc = d.currDoc;
    const markup = d.markup;
    const aliases = d.aliases;
    const saveUsersFields = [];
    let output = "";
    const errors = [];
    try {
        for (const user of data) {
            const tools = new Tools(user, currDoc, markup, aliases); // in compiled JS, this is tools_1.default(...)

            // Fake parameters hide the outer local variables so user script won't accidentally touch them.
            function runProgram(program: string, saveUsersFields?: never, output?: never, error?: never,
                                data?: never, d?: never, currDoc?: never, markup?: never, aliases?: never,
                                Tools?: never) {
                eval(program);
            }

            runProgram(d.program);
            saveUsersFields.push(tools.getResult());
            output += tools.getOutput();
            const userErrs = tools.getErrors();
            if (userErrs.length > 0) {
                errors.push({user: user.user.name, errors: userErrs});
            }
        }
        return {res: saveUsersFields, output, errors};
    } catch (e) {
        return {output, fatalError: e.message};
    }
}

router.put("/", (req, res, next) => {
    const decoded = JsrunnerAnswer.decode(req.body);
    if (decoded.isLeft()) {
        res.send({web: {error: "Incorrect input format."}});
        return;
    }
    const value = decoded.value;
    const regex = /[0-9]+\./;
    const currDoc = value.taskID.match(regex);
    if (!currDoc) {
        res.send({web: {error: "Taskid missing or incorrect format."}});
        return;
    }
    const isolate = new ivm.Isolate({
        memoryLimit: 128, // in MB
        inspector: false,
    });

    const toolsScript = isolate.compileScriptSync(toolsSource, {filename: "tools.js"});

    // Do not add more code inside the string. Edit the 'runner' function instead.
    const script = isolate.compileScriptSync(
        `
        const tools_1 = toolsjs;
        ${runner}
        JSON.stringify(runner(JSON.parse(g)))`,
        {
            filename: "script.js",
        }
    );
    const ctx = isolate.createContextSync({inspector: false});
    toolsScript.runSync(ctx);
    const runnerData: IRunnerData = {
        data: value.input.data,
        currDoc: currDoc[0],
        markup: value.markup,
        aliases: value.input.aliases,
        program: value.markup.program || "",
    };
    ctx.global.setSync("g", JSON.stringify(runnerData));
    const result: ReturnType<typeof runner> = JSON.parse(script.runSync(ctx, {timeout: 1000}));
    if (result.fatalError) {
        res.send({
            web: {
                fatalError: result.fatalError,
            },
        });
        return;
    }
    res.send({
        save: result.res,
        web: {
            result: "Script was executed.",
            print: result.output,
            errors: result.errors,
        },
    });
});

export default router;
