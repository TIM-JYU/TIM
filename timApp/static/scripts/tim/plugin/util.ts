import type {Type} from "io-ts/lib";
import type {ISetAnswerResult} from "tim/document/viewctrl";
import {FormModeOption} from "tim/document/viewctrl";
import {isLeft} from "fp-ts/lib/Either";
import type {IAnswer} from "tim/answer/IAnswer";
import type {IUser} from "tim/user/IUser";
import {createParContext} from "tim/document/structure/create";
import type {
    IGenericPluginMarkup,
    IGenericPluginTopLevelFields,
} from "tim/plugin/attributes";
import {getErrors} from "tim/plugin/errors";
import {TaskId} from "tim/plugin/taskid";

export function getDefaults<
    MarkupType extends IGenericPluginMarkup,
    A extends IGenericPluginTopLevelFields<MarkupType>,
    T extends Type<A, unknown>
>(runtimeType: T, defaultMarkup: MarkupType) {
    const defaults: IGenericPluginTopLevelFields<MarkupType> = {
        info: null,
        markup: defaultMarkup,
        preview: true,
        state: null,
    };
    const d = runtimeType.decode(defaults);
    if (isLeft(d)) {
        throw new Error(
            "Could not get default markup:\n" +
                getErrors(d)
                    .map((x) => `- ${x.name}: ${x.type}`)
                    .join("\n")
        );
    }
    return d.right;
}

export class PluginMeta {
    constructor(
        private readonly element: JQLite,
        private readonly preview = false,
        private readonly plugintype?: string,
        private readonly taskid?: string
    ) {}

    protected getParentAttr(name: string) {
        return this.element.parent().attr(name);
    }

    taskIdCache?: TaskId | null;

    public getTaskId() {
        if (this.taskIdCache !== undefined) {
            return this.taskIdCache ?? undefined;
        }
        const tidStr = this.taskid ?? this.getParentAttr("id");
        if (tidStr) {
            const r = TaskId.tryParse(tidStr);
            if (r.ok) {
                this.taskIdCache = r.result;
                return r.result;
            } else {
                this.taskIdCache = null;
                // If a plugin doesn't have task id, the servers sends "<docid>..<blockid>" in HTML.
                // Don't warn about those because they are common.
                // The server should be fixed to not put any taskid attribute in these cases.
                if (!r.result.startsWith("Task id has empty name:")) {
                    console.warn(r.result);
                }
            }
        }
    }

    protected getPlugin() {
        let p = this.plugintype ?? this.getParentAttr("plugin-type");
        if (!p) {
            const message = "Could not find plugin type from HTML";
            alert(message);
            throw new Error(message);
        }
        const i = p.indexOf("/");
        if (i >= 0) {
            p = p.substring(i + 1);
        }
        return p;
    }

    public getTaskIdUrl() {
        return `/${this.getPlugin()}/${this.getTaskId()!.docTask().toString()}`;
    }

    public getAnswerUrl() {
        return `${this.getTaskIdUrl()}/answer`;
    }

    public getIframeHtmlUrl(user: IUser, answer: IAnswer | undefined) {
        const start = `/iframehtml/${this.getPlugin()}/${this.getTaskId()!
            .docTask()
            .toString()}/${user.id}`;
        if (answer) {
            return `${start}/${answer.id}`;
        }
        return start;
    }

    public isPreview() {
        return this.preview;
    }
}

export type PluginMarkupErrors = Array<{name: string; type: string}>;

/**
 * Plugin base agnostic to the underlying framework.
 */
export abstract class PluginBaseCommon {
    abstract element: JQuery;
    protected abstract pluginMeta: PluginMeta;

    protected getRootElement() {
        return this.element[0];
    }

    isPreview() {
        return this.pluginMeta.isPreview();
    }

    getAreas(): string[] {
        const returnList: string[] = [];
        const parents = this.element.parents(".area");
        if (parents[0]) {
            const areaList = parents[0].classList;
            areaList.forEach((value) => {
                const m = value.match(/^area_(\S+)$/);
                if (m) {
                    returnList.push(m[1]);
                }
            });
        }
        return returnList;
    }

    belongsToArea(area: string): boolean {
        return this.getAreas().includes(area);
    }

    /**
     * Returns task name of the plugin.
     */
    getName() {
        return this.pluginMeta.getTaskId()?.name;
    }

    getTaskId() {
        return this.pluginMeta.getTaskId();
    }

    /**
     * Returns the plugin's parent paragraph.
     * If the plugin is not attached to a paragraph or the paragraph is not attached to a document, returns null.
     */
    public getPar() {
        const parElement = this.element.parents(".par")[0];
        if (!parElement.parentElement) {
            return undefined;
        }
        return createParContext(parElement);
    }

    /**
     * @returns {FormModeOption} FormModeOption.IsForm if plugin wants to register as formAnswerBrowser
     * This means invisible answerBrowser and direct answer input when changing users in ViewCtrl
     * If Undecided, let ViewCtrl decide if plugin is to be used as form.
     * IsForm and Undecided options should only be used by simple plugins where getState is not necessary when changing answers
     */
    public formBehavior(): FormModeOption {
        return FormModeOption.NoForm;
    }

    /**
     * Sets plugin's answer content via external call
     * @param content answer to be parsed
     * @returns ISetAnswerResult: {ok: boolean, message: (string | undefined)}
     * ok: true if content was successfully parsed
     * message: for replying with possible errors
     */
    setAnswer(content: Record<string, unknown>): ISetAnswerResult {
        return {ok: false, message: "Plugin doesn't support setAnswer"};
    }

    save() {
        throw Error(
            `Called unimplemented save on ${
                this.getTaskId()?.docTask().toString() ?? ""
            }`
        );
    }

    resetField(): undefined {
        return undefined;
    }

    resetChanges(): void {}
}

/**
 * Shuffles an array.
 * @param items Array of items to be shuffled.
 */
export function shuffle<T>(items: T[]): T[] {
    const result = items.slice();
    const n = items.length;
    for (let i = n - 1; i >= 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        const tmp = result[i];
        result[i] = result[j];
        result[j] = tmp;
    }
    return result;
}

/**
 * Converts plugin's "form" (boolean/undefined) attribute to FormModeOption
 * @param attr attribute to inspect
 * @param defBehavior default option to return if form attribute was not given
 */
export function getFormBehavior(
    attr: boolean | undefined,
    defBehavior: FormModeOption
): FormModeOption {
    if (attr == undefined) {
        return defBehavior;
    }
    return attr ? FormModeOption.IsForm : FormModeOption.NoForm;
}

export function inIframe() {
    try {
        return window.self !== window.top;
    } catch (e) {
        return true;
    }
}

/**
 * Parses "styles" from the plugin answer that were saved by tableForm
 * For now only backgroundColor is supported
 * @param styles style object to parse. If not supported, return empty object
 */
export function parseStyles(styles: Record<string, string>) {
    const output: Record<string, string> = {};
    if (!styles || Object.keys(styles).length == 0) {
        return output;
    }
    if (styles.backgroundColor) {
        output.backgroundColor = styles.backgroundColor;
    }
    return output;
}
