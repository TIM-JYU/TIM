import {IRootElementService, IScope} from "angular";
import * as t from "io-ts";
import {Type} from "io-ts/lib";
import {Binding} from "../util/utils";

export interface IPluginAttributes<Markup extends IGenericPluginMarkup, State> {
    markup: Markup; // not in csplugin?
    doLazy: boolean;
    anonymous: boolean;
    info: {};
    preview: boolean;
    show_result: boolean; // not in csplugin?
    state: State | null; // not in csplugin?
    targetFormat: string;
    taskID: string;
    taskIDExt: string;
    userPrint: boolean;
    // csplugin has these:
    // user_id: string
    // review: boolean
}

// Attributes that are valid for all plugins.
export const GenericPluginMarkup = t.partial({
    answerLimit: t.Integer,
    button: t.string,
    buttonText: t.string,
    footer: t.string,
    header: t.string,
    lazy: t.boolean,
    resetText: nullable(t.string),
    stem: t.string,
});

export interface IGenericPluginMarkup extends t.TypeOf<typeof GenericPluginMarkup> {
    // should be empty
}

export function getDefaults<MarkupType extends IGenericPluginMarkup,
    A extends {markup: MarkupType},
    T extends Type<A>>(runtimeType: T, defaultMarkup: MarkupType) {
    const d = runtimeType.decode({markup: defaultMarkup, info: null});
    if (d.isLeft()) {
        throw new Error("Could not get default markup");
    }
    return d.value;
}

// from io-ts readme
function getPaths<A>(v: t.Validation<A>): string[] {
    return v.fold((errors) => errors.map((error) => error.context.map(({key}) => key).join(".")), () => ["no errors"]);
}

/**
 * Base class for plugins.
 *
 * All properties or fields having a one-time binding in template should eventually return a non-undefined value.
 * That's why there are "|| null"s in several places.
 */
export abstract class PluginBase<MarkupType extends IGenericPluginMarkup, A extends {markup: MarkupType}, T extends Type<A>> {

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

    // Parsed form of json binding or default value if json was not valid.
    public attrsall: Readonly<A>;
    // Binding that has all the data as a JSON string.
    protected json!: Binding<string, "@">;

    protected markupError?: string;

    constructor(
        protected scope: IScope,
        protected element: IRootElementService) {
        this.attrsall = getDefaults(this.getAttributeType(), this.getDefaultMarkup());
    }

    abstract getDefaultMarkup(): Partial<MarkupType>;

    $postLink() {
    }

    $onInit() {
        const parsed = JSON.parse(atob(this.json)) as unknown;
        const validated = this.getAttributeType().decode(parsed);
        if (validated.isLeft()) {
            this.markupError = `Plugin has invalid markup values: ${getPaths(validated)}`;
        } else {
            this.attrsall = validated.value;
        }

        // These can be uncommented for debugging:
        // console.log(parsed);
        // console.log(this);
    }

    protected abstract getAttributeType(): T;

    protected getParentAttr(name: string) {
        return this.element.parent().attr(name);
    }

    protected getTaskId() {
        return this.getParentAttr("id");
    }

    protected getPlugin() {
        return this.getParentAttr("data-plugin");
    }

    protected getRootElement() {
        return this.element[0];
    }
}

// from https://github.com/teamdigitale/italia-ts-commons/blob/de4d85a2a1502da54f78aace8c6d7b263803f115/src/types.ts
export function withDefault<T extends t.Any>(
    type: T,
    defaultValue: t.TypeOf<T>,
): t.Type<t.TypeOf<T>, any> {
    return new t.Type(
        type.name,
        (v: any): v is T => type.is(v),
        (v: any, c: any) =>
            type.validate(v !== undefined && v !== null ? v : defaultValue, c),
        (v: any) => type.encode(v),
    );
}

export function nullable<T extends t.Any>(type: T) {
    return t.union([t.null, type]);
}
