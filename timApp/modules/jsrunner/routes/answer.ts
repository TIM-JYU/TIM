import express from "express";
import {readFileSync} from "fs";
import ivm from "isolated-vm";
import {AnswerReturn, ErrorList, IError, IJsRunnerMarkup} from "../public/javascripts/jsrunnertypes";
import {AliasDataT, JsrunnerAnswer, UserFieldDataT} from "../servertypes";
// import numberLines from "./runscript";
import Tools from "./tools";
import StatCounter from "./tools";

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

type RunnerResult =
    | {output: string, res: {}, errors: ErrorList, fatalError?: never, outdata: object}
    | {output: string, fatalError: IError};

/**
 * Runs jsrunner script with the provided data and returns the result.
 *
 * This function must NEVER be called outside isolate-vm!
 *
 * @param d The data to use.
 */
function runner(d: IRunnerData): RunnerResult {

    // TODO: This is allready in runscipt, but does not allow with 2 params???
    // And for some reason it is not found if it is outside this function???
    function numberLines2(s: string, delta: number): string {
        const lines = s.split("\n");
        let result = "";
        // tslint:disable-next-line: prefer-for-of
        for (let i = 0; i < lines.length; i++) {
            const space = (i + delta < 10) ? "0" : "";
            result += space + (i + delta) + ": " + lines[i] + "\n";
        }
        return result;
    }

    const data = d.data;
    const currDoc = d.currDoc;
    const markup = d.markup;
    const aliases = d.aliases;
    const saveUsersFields = [];
    // const statCounters: { [fieldname: string]: StatCounter } = {};
    const statCounters = {};
    let output = "";
    const errors = [];
    try {
        const guser = data[0];
        const gtools = new Tools(guser, currDoc, markup, aliases, statCounters); // create global tools
        // tslint:disable
        function runPreProgram(program: string | undefined, saveUsersFields?: never, output?: never, errors?: never,
                            data?: never, d?: never, currDoc?: never, markup?: never, aliases?: never) {
            if ( program ) eval(program);
        }
        runPreProgram(d.markup.preprogram);
        output += gtools.getOutput();
        gtools.clearOutput();

        for (const user of data) {
            const tools = new Tools(user, currDoc, markup, aliases, statCounters); // in compiled JS, this is tools_1.default(...)

            // Fake parameters hide the outer local variables so user script won't accidentally touch them.
            // tslint:disable
            function runProgram(program: string, saveUsersFields?: never, output?: never, errors?: never,
                                data?: never, d?: never, currDoc?: never, markup?: never, aliases?: never) {
                eval(program);
            }

            // tslint:enable
            runProgram(d.program);
            // tools.print("d", JSON.stringify(d));
            // tools.print("User", JSON.stringify(tools));
            saveUsersFields.push(tools.getResult());
            output += tools.getOutput();
            const userErrs = tools.getErrors();
            if (userErrs.length > 0) {
                errors.push({user: user.user.name, errors: userErrs});
            }
        }
        // tslint:disable
        function runPostProgram(program: string | undefined, saveUsersFields?: never, output?: never, errors?: never,
                            data?: never, d?: never, currDoc?: never, markup?: never, aliases?: never) {
            if ( program ) eval(program);
        }
        runPostProgram(d.markup.postprogram);
        saveUsersFields.push(gtools.getResult());
        output += gtools.getOutput();
        const guserErrs = gtools.getErrors();
        if (guserErrs.length > 0) {
            errors.push({user: guser.user.name, errors: guserErrs});
        }
        if ( errors.length > 0 ) {
            const prg = numberLines2(d.program, 1);
            errors.push( {
                user: "program",
                errors: [{
                    msg: "See program",
                    stackTrace: prg
                }]
            });
        }
        return {res: saveUsersFields, output, errors, outdata: gtools.outdata};
    } catch (e) {
        const err = e as Error;
        const prg = "\n" + numberLines2(d.program, 1);
        return {output, fatalError: {msg: err.message, stackTrace: e.stack + prg}};
    }
}

router.put("/", async (req, res, next) => {
    const decoded = JsrunnerAnswer.decode(req.body);
    if (decoded.isLeft()) {
        res.json({web: {error: "Invalid input to jsrunner answer route."}});
        return;
    }
    const value = decoded.value;
    const regex = /[0-9]+\./;
    const currDoc = value.taskID.match(regex);
    if (!currDoc) {
        res.json({web: {error: "Taskid missing or incorrect format."}});
        return;
    }
    const isolate = new ivm.Isolate({
        memoryLimit: 128, // in MB
        inspector: false,
    });

    const toolsScript = await isolate.compileScript(toolsSource, {filename: "tools.js"});

    // Do not add more code inside the string. Edit the 'runner' function instead.
    const script = await isolate.compileScript(
        `
        const tools_1 = toolsjs;
        ${runner}
        JSON.stringify(runner(JSON.parse(g)))`,
        {
            filename: "script.js",
        },
    );
    const ctx = await isolate.createContext({inspector: false});
    await toolsScript.run(ctx);
    const runnerData: IRunnerData = {
        data: value.input.data,
        currDoc: currDoc[0],
        markup: value.markup,
        aliases: value.input.aliases,
        program: value.markup.program || "",
    };
    await ctx.global.set("g", JSON.stringify(runnerData));
    let r: AnswerReturn;
    try {
        const result: ReturnType<typeof runner> = JSON.parse(
            await script.run(ctx, {timeout: value.markup.timeout || 1000}),
        );
        if (result.fatalError) {
            r = {
                web: {
                    fatalError: result.fatalError,
                    output: result.output,
                },
            };
        } else {
            r = {
                savedata: result.res,
                web: {
                    output: result.output,
                    errors: result.errors,
                    outdata: result.outdata,
                },
            };
        }
    } catch (e) {
        const err: Error = e;
        // This happens at least if the script execution times out.
        r = {
            web: {
                fatalError: {msg: err.message || "Unknown error occurred."},
                output: "",
            },
        };
    }
    res.json(r);
});

export default router;
