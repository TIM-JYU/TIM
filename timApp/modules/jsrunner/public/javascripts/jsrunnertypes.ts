import * as t from "io-ts";
import {GenericPluginMarkup, GenericPluginTopLevelFields} from "tim/plugin/attributes";

export const JsrunnerMarkup = t.intersection([
    t.partial({
        creditField: t.string,
        defaultPoints: t.number,
        docid: t.boolean,
        failGrade: t.string,
        fieldhelper: t.boolean,
        fields: t.array(t.string),
        gradeField: t.string,
        gradingScale: t.record(t.string, t.number),
        groups: t.array(t.string),
        program: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
    }),
]);

export interface IJsRunnerMarkup extends t.TypeOf<typeof JsrunnerMarkup> {
}

export const JsrunnerAll = t.intersection([
    GenericPluginTopLevelFields,
    t.type({markup: JsrunnerMarkup}),
]);
