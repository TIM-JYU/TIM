/**
 * Defines the client-side implementation of JavaScript runner plugin.
 */
import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, ElementRef, NgModule, NgZone} from "@angular/core";
import type {ITimComponent} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    Info,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {DomSanitizer} from "@angular/platform-browser";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {PurifyModule} from "tim/util/purify.module";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";

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
        editMode: withDefault(t.boolean, false),

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

@Component({
    selector: "tim-goaltable-runner",
    template: `
        <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
        <div [class.csRunDiv]="borders" [class.cs-has-header]="header" class="goalTableDiv no-popup-menu">
            <h4 *ngIf="header" [innerHtml]="header | purify"></h4>
            <div class="goalTableInner">
                <p>
                    <span *ngIf="stem" class="stem" [innerHtml]="stem | purify"></span>
                    <span *ngIf="bloomText" class="bloom" [innerHtml]="bloomText | purify"></span>
                </p>
                <label class="editText small hidden-print " [title]="editTitle">{{editText}}
                    <input type="checkbox" [(ngModel)]="editMode">
                </label>
                <table class="goaltable">
                    <tbody>
                        <tr class="heading">
                            <th>{{goalText}}</th>
                            <th *ngFor="let h of headings" [title]="getTitle(h)">{{h}}</th>
                        </tr>
                        <tr class="itemrow" *ngFor="let row of rows">
                            <td [innerHtml]="row.itemtext | purify"></td>
                            <td *ngFor="let h of headings" [ngStyle]="cellTDStyle(row, h)"
                                (click)="rbClicked(row, h)"
                            >
                                <span class="goalspan" [ngStyle]="cellStyle(row, h)" [innerHtml]="getCell(row, h)"></span>
                            </td>
                        </tr>
                    </tbody>
                </table>
                <div class="csRunMenuArea hidden-print">
                    <button class="timButton" [disabled]="isRunning || !isUnSaved()" (click)="save()"
                            *ngIf="editMode || isUnSaved()">
                        {{btnText}}
                    </button>&nbsp;
                    <span class="notSavedSpan" *ngIf="isUnSaved()"></span>&nbsp;
                    <span *ngIf="result">{{result}}</span>
                </div>
                <p *ngIf="footer" [innerHtml]="footer | purify" class="plgfooter"></p>
                <p></p>
            </div>
        </div>
`,
    styleUrls: ["goaltable-plugin.component.scss"],
})
export class GoalTablePluginComponent
    extends AngularPluginBase<
        t.TypeOf<typeof GoalTableMarkup>,
        t.TypeOf<typeof GoalTableAll>,
        typeof GoalTableAll
    >
    implements ITimComponent
{
    isRunning = false;
    private error: {message?: string; stacktrace?: string} = {};
    result: string = "";
    headings: string[] = [];
    rows: GoalLine[] = [];
    private minGoal: number = 0;
    private maxGoal: number = 6;
    private initGoal: number = 0;
    editMode: boolean = false;
    private initialValue: string = "";
    saveResponse: {saved: boolean; message: string | undefined} = {
        saved: false,
        message: undefined,
    };
    private content: string = "";
    bloomText: string = "";
    editText: string = "";
    goalText: string = "";
    btnText: string = "";
    editTitle: string = "";
    private lang: string = "fi";
    private scaleWords: string[] = [];

    constructor(
        el: ElementRef<HTMLElement>,
        http: HttpClient,
        domSanitizer: DomSanitizer,
        private zone: NgZone
    ) {
        super(el, http, domSanitizer);
    }

    getDefaultMarkup() {
        return {};
    }

    get borders() {
        return this.markup.borders;
    }

    ngOnInit() {
        super.ngOnInit();

        const aa = this.attrsall;
        const lang = this.markup.lang || "fi";
        this.lang = lang;
        const state = aa.state?.c ?? {};
        this.minGoal = this.markup.mingoal ?? 0;
        this.maxGoal = this.markup.maxgoal ?? scaleValueWords.length - 1;
        this.initGoal = this.markup.initgoal ?? 0;
        for (let i = this.minGoal; i <= this.maxGoal; i++) {
            this.headings.push("" + i);
        }
        for (const s of this.markup.goals ?? []) {
            const parts = s.split(";", 3);
            const iid = s.indexOf(";");
            const ig = s.indexOf(";", iid + 1);
            const id = parts[0].trim();
            const goal = parts[1].trim() || "0";
            const itemtext = s.substring(ig + 1).trim() || "";
            const userselection = state[id] || "" + this.initGoal;

            this.rows.push({
                id: id,
                goal: goal,
                itemtext: itemtext,
                userSelection: userselection,
            });
        }
        this.calcContent();
        this.initialValue = this.getContent();
        if (!this.attrsall.preview) {
            this.vctrl.addTimComponent(this);
        }

        // make translated words
        if (this.markup.goalscale) {
            this.scaleWords = this.markup.goalscale;
        } else {
            for (let i = 0; i < scaleValueWords.length; i++) {
                this.scaleWords[i] = scaleValueWords[i][lang];
            }
        }
        if (this.markup.bloom) {
            if (this.markup.lang === "en") {
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
        this.editText = this.markup.editText ?? goalTableWords.editText[lang];
        this.goalText = this.markup.goalText ?? goalTableWords.goalText[lang];
        this.editTitle = goalTableWords.editTitle[lang];
        this.editMode = this.markup.editMode;
    }

    ngOnDestroy() {
        if (!this.attrsall.preview) {
            this.vctrl.removeTimComponent(this);
        }
    }

    private getJSContent() {
        const c: Record<string, string> = {};
        const def: string = "" + this.initGoal;
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
        return this.zone.run(async () => {
            await this.saveText();
            return this.saveResponse;
        });
    }

    async saveText() {
        this.isRunning = true;
        const c = this.getJSContent();
        const params = {
            input: {
                nosave: false,
                c: c,
            },
        };

        this.error = {};
        this.result = "";

        const r = await this.postAnswer<{
            web?: {result?: string; error?: string};
            error?: string;
        }>(params);

        this.isRunning = false;
        if (!r.ok) {
            const e = r.result.error.error;
            if (e) {
                this.error.message = e;
                return;
            }
            this.error.message = r.result.error.error;
            return;
        }
        if (!r.result.web) {
            this.error.message = "No web reply from GoalTable!";
            return;
        }
        if (r.result.error) {
            this.error.message = r.result.error;
            return;
        }
        if (r.result.web.error) {
            this.error.message = r.result.web.error;
            return;
        }
        if (r.result.web.result) {
            this.result = r.result.web.result;
            this.initialValue = this.getContent();
            return;
        }
    }

    rbClicked(row: GoalLine, h: string) {
        if (!this.editMode) {
            return;
        }
        row.userSelection = h;
        this.calcContent();
        this.result = "";
    }

    // noinspection JSMethodCanBeStatic,JSUnusedLocalSymbols
    cellStyle(row: GoalLine, h: string) {
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
    cellTDStyle(row: GoalLine, h: string) {
        const styles: Record<string, string> = {};
        if (row.goal === h) {
            styles["background-color"] = "#ffff00";
        }
        return styles;
    }

    getCell(row: GoalLine, h: string) {
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

    getTitle(h: string) {
        const title: string = h + " = " + this.scaleWords[+h];
        return title;
    }
}

@NgModule({
    declarations: [GoalTablePluginComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        TimUtilityModule,
        FormsModule,
        TooltipModule.forRoot(),
        PurifyModule,
    ],
})
export class GoalTableModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("goaltable-runner", GoalTableModule, GoalTablePluginComponent);
