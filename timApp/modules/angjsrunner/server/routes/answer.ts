import {readFileSync} from "fs";
import {parse} from "acorn";
import express from "express";
import ivm from "isolated-vm";
import {isLeft} from "fp-ts/lib/Either";

// import numberLines from "./runscript";
import type {Branded, IntBrand} from "io-ts";
import type {
    AnswerReturn,
    ErrorList,
    IError,
    IGroupData,
    IItemRightActionData,
    IJsRunnerMarkup,
    IMailSendData,
} from "../../shared/jsrunnertypes";
import type {
    AliasDataT,
    PeerReviewDataT,
    UserFieldDataT,
    VelpDataT,
} from "../servertypes";
import {JsrunnerAnswer} from "../servertypes";
import {ivmRequest} from "../util/request";
import type {
    IToolsResult,
    NewUserData,
    SendGradesToSisuData,
    ToolsBase,
} from "./tools";
import {GTools, numberLines, Tools} from "./tools";

console.log("answer");
const router = express.Router();

// https://stackoverflow.com/questions/21856097/how-can-i-check-javascript-code-for-syntax-errors-only-from-the-command-line
// https://github.com/acornjs/acorn/tree/master/acorn#interface
function compileProgram(code: string): string {
    try {
        const ret = parse(code, {ecmaVersion: 2020});
        // eslint-disable-next-line @typescript-eslint/no-base-to-string
        return ret.toString();
    } catch (e) {
        const err = e as Error;
        return err.message;
    }
}

type Users = Record<string | number, string>;

// Every time tools.ts (or some of its dependencies) is modified, dist/tools.js must be
// rebuilt with 'npm run buildtools'.
const toolsSource = readFileSync("./dist/tools.js").toString();

interface IRunnerData {
    aliases: AliasDataT;
    currDoc: string;
    data: UserFieldDataT[];
    markup: IJsRunnerMarkup;
    program: string;
    compileProgram: (code: string) => string;
    velps: VelpDataT[];
    peerreviews: PeerReviewDataT[] | null;
}

type RunnerResult =
    | {
          output: string;
          res: IToolsResult[];
          errors: ErrorList;
          fatalError?: never;
          outdata: Record<string, unknown>;
          groups: IGroupData;
          itemRightActions: IItemRightActionData[];
          newUsers: NewUserData[];
          mailToSend: IMailSendData[];
          sendSisuAssessments?: SendGradesToSisuData;
      }
    | {output: string; fatalError: IError; errorprg: string};

/**
 * Runs jsrunner script with the provided data and returns the result.
 *
 * This function must NEVER be called outside isolate-vm!
 *
 * @param d The data to use.
 */
function runner(d: IRunnerData): RunnerResult {
    let errorprg: string | undefined = "";
    let prgname: string | undefined = "";
    const data = d.data;
    const currDoc = d.currDoc;
    const markup = d.markup;
    const aliases = d.aliases;
    const velps = d.velps;
    const peerreviews = d.peerreviews;
    const saveUsersFields: IToolsResult[] = [];
    // const statCounters: { [fieldname: string]: StatCounter } = {};
    let output = "";
    const errors = [];
    try {
        // const guser1 = data[0];
        // const guser2 = data[data.length - 1];

        const dummyUser = {
            user: {
                id: -1 as Branded<number, IntBrand>,
                name: "pre/post",
                real_name: "",
                email: "",
            },
            groupinfo: {
                membership_add: null,
                membership_end: null,
                active_memberships: [],
                expired_memberships: [],
            },
            fields: {},
            styles: {},
        };
        const dummyTools = new Tools(
            dummyUser,
            {},
            currDoc,
            markup,
            aliases,
            velps,
            peerreviews
        ); // in compiled JS, this is tools_1.default(...)
        const gtools = new GTools(
            currDoc,
            markup,
            aliases,
            dummyTools,
            saveUsersFields
        ); // create global tools
        // Fake parameters hide the outer local variables so user script won't accidentally touch them.
        /* eslint-disable @typescript-eslint/no-shadow,no-eval */
        function runProgram(
            program: string,
            pname: string,
            tools: ToolsBase,
            saveUsersFields?: never,
            output?: never,
            errors?: never,
            data?: never,
            d?: never,
            currDoc?: never,
            markup?: never,
            aliases?: never
        ) {
            errorprg = program;
            prgname = pname;
            eval(`function main() {${program}\n} main();`);
        }
        /* eslint-enable @typescript-eslint/no-shadow,no-eval */
        if (d.markup.preprogram) {
            runProgram(d.markup.preprogram, "preprogram", gtools);
        }
        output += gtools.getOutput();
        gtools.clearOutput();
        const users: Users = {};
        data.forEach((user) => {
            users[user.user.id] = user.user.name;
        });
        for (const user of data) {
            const tools = new Tools(
                user,
                users,
                currDoc,
                markup,
                aliases,
                velps,
                peerreviews
            ); // in compiled JS, this is tools_1.default(...)
            tools.usePrintLine = gtools.usePrintLine;
            gtools.setTools(tools);
            errorprg = "gtools.addToDatas(tools)";
            prgname = "addToDatas";
            if (d.markup.autoadd) {
                gtools.addToDatas();
            }
            runProgram(d.program, "program", tools);
            gtools.usePrintLine = tools.usePrintLine;
            // tools.print("d", JSON.stringify(d));
            // tools.print("User", JSON.stringify(tools));
            saveUsersFields.push(tools.getResult());
            output += tools.getOutput();
            const userErrs = tools.getErrors();
            if (userErrs.length > 0) {
                errors.push({user: user.user.name, errors: userErrs});
            }
        }

        // dtools = new Tools(guser2, "", markup, aliases); // in compiled JS, this is tools_1.default(...)
        gtools.setTools(dummyTools);
        gtools.clearGtools(); // to avoid circular references if print gtools
        if (d.markup.postprogram) {
            runProgram(d.markup.postprogram, "postprogram", gtools);
        }
        // saveUsersFields.push(gtools.getResult());
        output += gtools.getOutput();
        const guserErrs = gtools.getErrors();
        if (guserErrs.length > 0) {
            errors.push({user: dummyUser.user.name, errors: guserErrs});
        }
        if (guserErrs.length > 0) {
            // TODO: separate errors from pre, program and post
            const prg = "\n" + prgname + ":\n" + numberLines(errorprg, 1);
            errors.push({
                user: "program",
                errors: [
                    {
                        msg: "See program",
                        stackTrace: prg,
                    },
                ],
            });
        }
        return {
            res: saveUsersFields,
            output,
            errors,
            outdata: gtools.outdata,
            groups: gtools.groups,
            itemRightActions: gtools.itemRightActions,
            newUsers: gtools.newUsers,
            mailToSend: gtools.mailToSend,
            sendSisuAssessments: gtools.sendToSisu,
        };
    } catch (e) {
        const err = e as Error;
        const prg = prgname + ":\n" + numberLines(errorprg, 1);
        // const comp = ""; // d.compileProgram(errorprg);
        // prg = "\n" + comp + prg;
        let stack = err.stack ?? "";
        if (stack.includes("SyntaxError")) {
            stack = ""; // compile will show the errorplace
        } else {
            errorprg = "";
            const ano = "<anonymous>";
            let i1 = stack.indexOf(ano);
            if (i1 >= 0) {
                i1 += ano.length;
                let i2 = stack.indexOf(")", i1);
                if (i2 < 0) {
                    i2 = stack.length - 1;
                }
                stack = "Index (" + stack.substring(i1 + 1, i2 + 1) + "\n";
            }
        }
        return {
            output,
            fatalError: {msg: err.message, stackTrace: stack + prg},
            errorprg,
        };
    }
}

// noinspection JSUnusedLocalSymbols
router.put("/", async (req, res, next) => {
    const decoded = JsrunnerAnswer.decode(req.body);
    if (isLeft(decoded)) {
        const rep = decoded.left.map((err) => {
            const ctx = err.context[err.context.length - 1];
            return {
                actual: ctx.actual,
                key: ctx.key,
                type: ctx.type.name,
            };
        });
        res.json({
            web: {
                error: "Invalid inputs to jsrunner answer route",
                invalidInputs: rep,
            },
        });
        return;
    }
    const value = decoded.right;
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

    const toolsScript = await isolate.compileScript(toolsSource, {
        filename: "tools.js",
    });

    // Do not add more code inside the string. Edit the 'runner' function instead.
    const script = await isolate.compileScript(
        `
        const tools_1 = toolsjs;
        ${runner.toString()}
        JSON.stringify(runner(JSON.parse(g)))`,
        {
            filename: "script.js",
        }
    );
    const ctx = await isolate.createContext({inspector: false});
    await toolsScript.run(ctx);
    const runnerData: IRunnerData = {
        data: value.input.data,
        currDoc: currDoc[0],
        markup: value.markup,
        aliases: value.input.aliases,
        program: value.markup.program ?? "",
        compileProgram: compileProgram,
        velps: value.input.velps,
        peerreviews: value.input.peerreviews,
    };
    await ctx.global.set("g", JSON.stringify(runnerData));
    await ctx.global.set(
        "_ivm_request",
        new ivm.Callback(ivmRequest, {sync: true})
    );
    let r: AnswerReturn;
    try {
        const result: ReturnType<typeof runner> = JSON.parse(
            await script.run(ctx, {timeout: value.markup.timeout ?? 1000})
        );

        // console.log(JSON.stringify(result));

        if (result.fatalError) {
            const rese = result;
            if (rese.errorprg) {
                // TODO: compile here, because I could not do it in runner???
                const err = compileProgram(rese.errorprg);
                result.fatalError.stackTrace =
                    err + "\n" + result.fatalError.stackTrace;
            }
            r = {
                web: {
                    fatalError: result.fatalError,
                    output: result.output,
                },
            };
        } else {
            r = {
                savedata: result.res,
                groups: result.groups,
                itemRightActions: result.itemRightActions,
                newUsers: result.newUsers,
                mailToSend: result.mailToSend,
                sendSisuAssessments: result.sendSisuAssessments,
                web: {
                    output: result.output,
                    errors: result.errors,
                    outdata: result.outdata,
                },
            };
        }
    } catch (e) {
        const err = e as Error;
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
