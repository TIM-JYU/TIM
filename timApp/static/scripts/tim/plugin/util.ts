import {IController, IRootElementService, IScope} from "angular";
import {Type} from "io-ts/lib";
import {Binding} from "../util/utils";
import {IGenericPluginMarkup, IGenericPluginTopLevelFields} from "./attributes";
import {getErrors} from "./errors";

export function getDefaults<MarkupType extends IGenericPluginMarkup,
    A extends IGenericPluginTopLevelFields<MarkupType>,
    T extends Type<A>>(runtimeType: T, defaultMarkup: MarkupType) {
    const defaults: IGenericPluginTopLevelFields<MarkupType> = {
        info: null,
        markup: defaultMarkup,
        preview: true,
        state: null,
    };
    const d = runtimeType.decode(defaults);
    if (d.isLeft()) {
        throw new Error("Could not get default markup");
    }
    return d.value;
}

export class PluginMeta {
    constructor(
        private element: IRootElementService,
        private preview = false,
        private plugintype?: string,
        private taskid?: string,
    ) {

    }

    protected getParentAttr(name: string) {
        return this.element.parent().attr(name);
    }

    public getTaskId() {
        return this.taskid || this.getParentAttr("id");
    }

    protected getPlugin() {
        return this.plugintype || this.getParentAttr("data-plugin");
    }

    public getAnswerUrl() {
        const plugin = this.getPlugin();
        if (!plugin) {
            const message = "Could not find plugin type from HTML";
            alert(message);
            throw new Error(message);
        }
        let url = plugin;
        const i = url.lastIndexOf("/");
        if (i > 0) {
            url = url.substring(i);
        }
        url += `/${this.getTaskId()}/answer/`;
        return url; // + window.location.search;
    }

    public isPreview() {
        return this.preview;
    }
}

/**
 * Base class for plugins.
 *
 * All properties or fields having a one-time binding in template should eventually return a non-undefined value.
 * That's why there are "|| null"s in several places.
 */
export abstract class PluginBase<MarkupType extends IGenericPluginMarkup, A extends IGenericPluginTopLevelFields<MarkupType>, T extends Type<A>> implements IController {
    static $inject = ["$scope", "$element"];

    buttonText() {
        return this.attrs.button || this.attrs.buttonText || null;
    }

    get attrs(): Readonly<MarkupType> {
        return this.attrsall.markup;
    }

    get footer() {
        return this.attrs.footer || null;
    }

    get header() {
        return this.attrs.header || null;
    }

    get stem() {
        return this.attrs.stem || null;
    }

    /**
     * Returns if this plugin is readonly for the current user.
     */
    get readonly(): boolean {
        return this.attrsall.access === "readonly";
    }

    // Parsed form of json binding or default value if json was not valid.
    public attrsall: Readonly<A>;
    // Binding that has all the data as a JSON string.
    protected json!: Binding<string, "@">;

    // Optional bindings that are used when the plugin is compiled without being attached to document.
    // In that case, the plugin element does not have the parent where to fetch the type and task id, so they
    // are provided when compiling.
    protected plugintype?: Binding<string, "@?">;
    protected taskid?: Binding<string, "@?">;

    protected markupError?: Array<{ name: string, type: string }>;
    protected pluginMeta: PluginMeta;

    constructor(
        protected scope: IScope,
        protected element: IRootElementService) {
        this.attrsall = getDefaults(this.getAttributeType(), this.getDefaultMarkup());
        this.pluginMeta = new PluginMeta(element, this.attrsall.preview);
    }

    abstract getDefaultMarkup(): Partial<MarkupType>;

    $postLink() {

    }

    $onInit() {
        const parsed = JSON.parse(atob(this.json)) as unknown;
        const validated = this.getAttributeType().decode(parsed);
        if (validated.isLeft()) {
            this.markupError = getErrors(validated);
        } else {
            this.attrsall = validated.value;
            this.pluginMeta = new PluginMeta(
                this.element,
                this.attrsall.preview,
                this.plugintype,
                this.taskid,
            );
        }

        // These can be uncommented for debugging:
        // console.log(parsed);
        // console.log(this);
    }

    protected abstract getAttributeType(): T;

    protected getRootElement() {
        return this.element[0];
    }

    getAreas(): string[] {
        const returnList: string[] = [];
        const parents = this.element.parents(".area");
        if (parents[0]) {
            const areaList = parents[0].classList;
            areaList.forEach(
                (value) => {
                    const m = value.match(/^area_(\S+)$/);
                    if (m) {
                        returnList.push(m[1]);
                    }
                },
            );
        }
        return returnList;
    }

    belongsToArea(area: string): boolean {
        return this.getAreas().includes(area);
    }

    /**
     * Returns the name given to the plugin.
     */
    getName(): string | undefined {
        const taskId = this.pluginMeta.getTaskId();
        if (taskId) { return taskId.split(".")[1]; }
    }

    getTaskId(): string | undefined {
        const taskId = this.pluginMeta.getTaskId();
        if (taskId) {
            const docTask = taskId.split(".");
            return docTask[0].toString() + "." + docTask[1].toString();
        }
    }

    /**
     * Returns the plugin's parent paragraph.
     */
    public getPar() {
        return this.element.parents(".par");
    }

    /**
     * @returns {Boolean} true if plugin supports setAnswer
     * False by default
     */
    public supportsSetAnswer(): boolean {
        return false;
    }

    /**
     * @returns {Boolean} true if plugin wants to register as formAnswerBrowser
     * This mean invisible answerBrowser and direct answer input when changing users in viewCtrl
     * Should only be used by simple plugins where getState is not necessary when changing answers
     */
    public isForm(): boolean {
        return false;
    }

    /**
     * Sets plugin's answer content via external call
     * @param content answer to be parsed
     * @returns {ok: boolean, message: (string | undefined)}
     * ok: true if content was succesfully parsed
     * message: for replying with possible errors
     * TODO: This could be integrated into isForm
     */
    setAnswer(content: { [index: string]: unknown }): { ok: boolean, message: (string | undefined) } {
        return {ok: false, message: "Plugin doesn't support setAnswer"};
    }

    resetField(): undefined {
        // this.$onInit()
        return undefined;
    }
}

/**
 * Shuffles a string array.
 * @param strings Array of strings to be shuffled.
 */
export function shuffleStrings(strings: string []): string [] {
    const result = strings.slice();
    const n = strings.length;
    for (let i = n - 1; i >= 0; i--) {
        const j = Math.floor(Math.random() * (i + 1));
        const tmp = result[i];
        result[i] = result[j];
        result[j] = tmp;
    }
    return result;
}

export const pluginBindings = {
    json: "@",
    plugintype: "@?",
    taskid: "@?",
};
