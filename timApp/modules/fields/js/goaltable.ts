/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import angular from "angular";
import * as t from "io-ts";
import {ITimComponent, ViewCtrl} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {$http} from "tim/util/ngimport";
import {defaultTimeout, to} from "tim/util/utils";

const goalTableApp = angular.module("goalTableApp", ["ngSanitize"]);
export const moduleDefs = [goalTableApp];

// this.attrs
const GoalTableMarkup = t.intersection([
    t.partial({
        goals: t.array(t.string),
        goalscale: t.array(t.string),
        editText: t.string,
        goalText: t.string,
        mingoal: t.number,
        maxgoal: t.number,
        initgoal: t.number,
    }),
    GenericPluginMarkup,
    t.type({
        borders: withDefault(t.boolean, true),
        bloom: withDefault(t.boolean, true),
        lang: withDefault(t.string, "fi"),
        // autoplay: withDefault(t.boolean, true),
        // file: t.string,
        // open: withDefault(t.boolean, false),
    }),
]);
const GoalTableAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: GoalTableMarkup,
        preview: t.boolean,
        state: nullable(t.type({c: t.record(t.string, t.string)})),
    }),
]);

interface GoalLine {
    id: string;
    goal: string;
    itemtext: string;
    userSelection: string;
}

type Word = Record<string, string>;

type Words = Record<string, Word>;

const goalTableWords: Words = {
    btnText: {fi: "Tallenna", en: "Save"},
    editText: {fi: "Muokkaa", en: "Modify"},
    goalText: {fi: "Osattava asia", en: "Learning outcomes"},
    editTitle: {
        fi: "Ruksi jotta voit siirellä",
        en: "Check this to move items",
    },
};
const scaleValueWords: Word[] = [
    {fi: "ei kuullut", en: "never heard"}, // 0
    {fi: "muistaa", en: "remember"}, // 1
    {fi: "ymmärtää", en: "understands"}, // 2
    {fi: "osaa soveltaa", en: "apply"}, // 3
    {fi: "osaa analysoida", en: "analyze"}, // 4
    {fi: "osaa arvioida", en: "evaluate"}, // 5
    {fi: "osaa luoda", en: "create"}, // 6
];

class GoalTableController
    extends PluginBase<
        t.TypeOf<typeof GoalTableMarkup>,
        t.TypeOf<typeof GoalTableAll>,
        typeof GoalTableAll
    >
    implements ITimComponent {
    private vctrl!: ViewCtrl;
    private isRunning = false;
    private error: {message?: string; stacktrace?: string} = {};
    private result: string = "";
    // noinspection JSMismatchedCollectionQueryUpdate
    private headings: string[] = [];
    // noinspection JSMismatchedCollectionQueryUpdate
    private rows: GoalLine[] = [];
    private mingoal: number = 0;
    private maxgoal: number = 6;
    private initgoal: number = 0;
    private editMode: boolean = false;
    private initialValue: string = "";
    private saveResponse: {saved: boolean; message: string | undefined} = {
        saved: false,
        message: undefined,
    };
    private content: string = "";
    private bloomText: string = "";
    private editText: string = "";
    private goalText: string = "";
    private btnText: string = "";
    private editTitle: string = "";
    private lang: string = "fi";
    private scaleWords: string[] = [];

    getDefaultMarkup() {
        return {};
    }

    $onInit() {
        super.$onInit();
        const aa = this.attrsall;
        const lang = this.attrs.lang || "fi";
        this.lang = lang;
        const state = aa.state?.c ?? {};
        this.mingoal = this.attrs.mingoal ?? 0;
        this.maxgoal = this.attrs.maxgoal ?? scaleValueWords.length - 1;
        this.initgoal = this.attrs.initgoal ?? 0;
        for (let i = this.mingoal; i <= this.maxgoal; i++) {
            this.headings.push("" + i);
        }
        for (const s of this.attrs.goals ?? []) {
            const parts = s.split(";", 3);
            const iid = s.indexOf(";");
            const ig = s.indexOf(";", iid + 1);
            const id = parts[0].trim();
            const goal = parts[1].trim() || "0";
            const itemtext = s.substring(ig + 1).trim() || "";
            const userselection = state[id] || "" + this.initgoal;

            this.rows.push({
                id: id,
                goal: goal,
                itemtext: itemtext,
                userSelection: userselection,
            });
        }
        this.calcContent();
        this.initialValue = this.getContent();
        this.vctrl.addTimComponent(this);

        // make translated words
        if (this.attrs.goalscale) {
            this.scaleWords = this.attrs.goalscale;
        } else {
            for (let i = 0; i < scaleValueWords.length; i++) {
                this.scaleWords[i] = scaleValueWords[i][lang];
            }
        }
        if (this.attrs.bloom) {
            if (this.attrs.lang === "en") {
                this.bloomText = "(learning outcomes by Bloom's taxonomy: ";
            } else {
                this.bloomText =
                    "(osaamisen taso sovelletulla Bloomin asteikolla: ";
            }
            let sep = "";
            for (let i = 1; i < this.scaleWords.length; i++) {
                this.bloomText += sep + i + "=" + this.scaleWords[i];
                sep = ", ";
            }
            this.bloomText += ")";
        }
        this.btnText = super.buttonText() ?? goalTableWords.btnText[lang];
        this.editText = this.attrs.editText ?? goalTableWords.editText[lang];
        this.goalText = this.attrs.goalText ?? goalTableWords.goalText[lang];
        this.editTitle = goalTableWords.editTitle[lang];
    }

    private getJSContent() {
        const c: Record<string, string> = {};
        const def: string = "" + this.initgoal;
        for (const row of this.rows) {
            const u = row.userSelection;
            if (u !== def) {
                c[row.id] = u;
            }
        }
        return c;
    }

    private calcContent() {
        this.content = JSON.stringify(this.getJSContent());
        return this.content;
    }

    getAttributeType() {
        return GoalTableAll;
    }

    /**
     * Returns (user) content in string form.
     */
    getContent(): string {
        return this.content;
    }

    // noinspection JSUnusedGlobalSymbols
    /**
     * Checking if input has been changed since the last Save or initialization.
     * Displays a red thick marker at the right side of the inputfield to notify users
     * about unsaved changes.
     * Unused method warning is suppressed, as the method is only called in template.
     */
    isUnSaved() {
        const unsaved = this.initialValue !== this.getContent();
        if (unsaved) {
            // this.hideSavedText = true;
        }
        return unsaved;
    }

    /**
     * Save method for other plugins, needed by e.g. multisave plugin.
     */
    async save() {
        await this.saveText();
        return this.saveResponse;
    }

    // noinspection JSUnusedGlobalSymbols
    async saveText() {
        this.isRunning = true;
        const url = this.pluginMeta.getAnswerUrl();
        // url.replace("answer", "goalTable");
        const c = this.getJSContent();
        const params = {
            input: {
                nosave: false,
                c: c,
            },
        };

        this.error = {};
        this.result = "";

        const r = await to(
            $http<{
                web?: {result?: string; error?: string};
                error?: string;
            }>({method: "PUT", url: url, data: params, timeout: defaultTimeout})
        );

        this.isRunning = false;
        if (!r.ok) {
            const e = r.result.data.error;
            if (e) {
                this.error.message = e;
                return;
            }
            this.error.message = r.result.data.error;
            return;
        }
        if (!r.result.data.web) {
            this.error.message = "No web reply from GoalTable!";
            return;
        }
        if (r.result.data.error) {
            this.error.message = r.result.data.error;
            return;
        }
        if (r.result.data.web.error) {
            this.error.message = r.result.data.web.error;
            return;
        }
        if (r.result.data.web.result) {
            this.result = r.result.data.web.result;
            this.initialValue = this.getContent();
            return;
        }
    }

    // noinspection JSUnusedLocalSymbols
    private rbClicked(row: GoalLine, h: string) {
        if (!this.editMode) {
            return;
        }
        row.userSelection = h;
        this.calcContent();
        this.result = "";
    }

    // noinspection JSMethodCanBeStatic,JSUnusedLocalSymbols
    private cellStyle(row: GoalLine, h: string) {
        const styles: Record<string, string> = {};
        const userSelection: string = row.userSelection;
        if (row.goal <= userSelection && userSelection === h) {
            styles["background-color"] = "#00ff00";
        } else if (row.goal === h) {
            styles["background-color"] = "#ffff00";
        } else if (userSelection === h) {
            styles["background-color"] = "#ffb0b0";
        }
        return styles;
    }

    // noinspection JSMethodCanBeStatic,JSUnusedLocalSymbols
    private cellTDStyle(row: GoalLine, h: string) {
        const styles: Record<string, string> = {};
        if (row.goal === h) {
            styles["background-color"] = "#ffff00";
        }
        return styles;
    }

    private getCell(row: GoalLine, h: string) {
        let html = "&nbsp;";
        const userSelection: string = row.userSelection;
        /* if ( row.goal <= userSelection && userSelection === h) {
            // html = ".";
        } else */
        if (row.goal === h) {
            html = "o";
        } else if (userSelection === h) {
            //
        }
        return html;
    }

    private getTitle(h: string) {
        const title: string = h + " = " + this.scaleWords[+h];
        return title;
    }
}

goalTableApp.component("goaltableRunner", {
    bindings: pluginBindings,
    controller: GoalTableController,
    require: {
        vctrl: "^timView",
    },
    template: `
<tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
<div ng-cloak ng-class="{'csRunDiv': ($ctrl.attrs.borders )}" class="goalTableDiv no-popup-menu" >
   <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
   <div class="goalTableInner">
    <p>
    <stem ng-if="::$ctrl.stem" class="stem" ng-bind-html="::$ctrl.stem"></stem>
    <stem ng-if="::$ctrl.bloomText" class="bloom" ng-bind-html="::$ctrl.bloomText"></stem>
    </p>
        <label class="editText small hidden-print " ng-attr-title="{{::$ctrl.editTitle}}">{{::$ctrl.editText}}
         <input type="checkbox" ng-model="$ctrl.editMode">
         </label>
    <table class="goaltable">
      <tr class="heading">
        <th>{{::$ctrl.goalText}}</th>
        <th ng-repeat="$h in $ctrl.headings"  ng-attr-title="{{::$ctrl.getTitle($h)}}">{{::$h}}</th>
      </tr>
      <tr class="itemrow" ng-repeat="$row in $ctrl.rows" >
        <td ng-bind-html="::$row.itemtext"></td>
        <td ng-repeat="$h in $ctrl.headings" ng-style="::$ctrl.cellTDStyle($row, $h)"
            ng-click="$ctrl.rbClicked($row, $h)"
        >
          <span class="goalspan" ng-style="$ctrl.cellStyle($row, $h)" ng-bind-html="::$ctrl.getCell($row, $h)"  ></span>
          <!-- <input type="radio" ng-model="$row.id" ng-value="$h" ng-click="$ctrl.rbClicked($row, $h)"> -->
        </td>
      </tr>
    </table>
    <div class="csRunMenuArea hidden-print" >
        <button class="timButton"  ng-disabled="$ctrl.isRunning || !$ctrl.isUnSaved()" ng-click="$ctrl.save()" ng-show="$ctrl.editMode || $ctrl.isUnSaved()">
            {{::$ctrl.btnText}}
        </button>
        <span class = "notSavedSpan" ng-show="$ctrl.isUnSaved()"></span>
        <span ng-if="$ctrl.result">{{$ctrl.result}}</span>
    </div>
    <pre ng-if="$ctrl.output">{{$ctrl.output}}</pre>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
    <p></p>
   </div>
</div>
`,
});
