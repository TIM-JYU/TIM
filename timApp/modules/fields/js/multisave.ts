/**
 * Defines the client-side implementation of a plugin that calls other plugins' save methods.
 */
import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, ElementRef, NgModule, NgZone} from "@angular/core";
import type {
    IChangeListener,
    ITimComponent,
    ViewCtrl,
} from "tim/document/viewctrl";
import {ChangeType, RegexOption} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    IncludeUsersOption,
    Info,
    withDefault,
} from "tim/plugin/attributes";
import {escapeRegExp, scrollToElement, timeout} from "tim/util/utils";
import type {TaskId} from "tim/plugin/taskid";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {HttpClient, HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {DomSanitizer} from "@angular/platform-browser";
import {showConfirm} from "tim/ui/showConfirmDialog";
import {slugify} from "tim/util/slugify";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {CommonModule} from "@angular/common";
import {PurifyModule} from "tim/util/purify.module";
import {
    GroupType,
    SisuAssessmentExportModule,
} from "./sisu-assessment-export.component";

const multisaveMarkup = t.intersection([
    t.partial({
        aliases: t.record(t.string, t.string),
        allSavedText: t.string,
        areas: t.array(t.string),
        tags: t.array(t.string),
        fields: t.array(t.string),
        followid: t.string,
        group: GroupType,
        jumplink: t.string,
        jumptarget: t.string,
        destCourse: t.string,
        includeUsers: IncludeUsersOption,
        testOnly: t.boolean,
        savedText: t.string,
        unsavedText: t.string,
        timer: t.number,
        saveDelay: t.number,
        showMessages: t.boolean,
        messageOverrides: t.array(
            t.type({
                expect: t.string,
                replace: t.string,
            })
        ),
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        autoUpdateDuplicates: withDefault(t.boolean, true),
        autoUpdateTables: withDefault(t.boolean, true),
        nosave: withDefault(t.boolean, false),
        livefeed: withDefault(t.boolean, false),
    }),
]);
const multisaveAll = t.intersection([
    t.partial({}),
    t.type({
        info: Info,
        markup: multisaveMarkup,
        preview: t.boolean,
    }),
]);

// noinspection TypeScriptUnresolvedVariable
@Component({
    selector: "tim-multisave",
    template: `
        <span class="no-popup-menu">
    <tim-markup-error *ngIf="markupError" [data]="markupError"></tim-markup-error>
    <h4 *ngIf="header" [innerHtml]="header"></h4>
    <tim-sisu-assessment-export
            *ngIf="markup.destCourse"
            [docId]="vctrl.item.id"
            [destCourse]="markup.destCourse"
            [includeUsers]="markup.includeUsers"
            [testOnly]="markup.testOnly"
            [group]="markup.group">
    </tim-sisu-assessment-export>
    <div *ngIf="livefeed"> <!-- unsaved fields -->
        <div *ngIf="!allSaved()">
            {{unsavedText}}
            <ul>
                <li *ngFor="let tag of unsavedTasksWithAliases">
                    <a (click)="scrollTo(tag.component); $event.preventDefault()">{{tag.alias}}</a>
                </li>
            </ul>
        </div>
        <div *ngIf="saveMessages" class="saveMessages">
            <div *ngFor="let m of saveMessages">{{m}}</div>
        </div>
        <div *ngIf="allSaved()">
            {{allSavedText}}
        </div>
    </div> <!-- unsaved fields -->
    <div *ngIf="!livefeed || !allSaved()">
    <button class="timButton"
            [disabled]="(disableUnchanged && allSaved())"
            *ngIf="buttonText() && !markup.destCourse"
            (click)="save()">
        {{buttonText()}}
    </button>
    &nbsp;
    <button class="btn btn-default"
            *ngIf="(undoButton && !allSaved())"
            [title]="undoTitle"
            (click)="tryResetChanges($event)">
        {{undoButton}}
    </button>
    <p class="savedtext" *ngIf="isSaved && allSaved()">{{savedText}}</p>
    </div>
    <p *ngIf="footer" [innerHtml]="footer | purify" class="plgfooter"></p>
</span>
    `,
    styleUrls: ["./multisave.component.scss"],
})
export class MultisaveComponent
    extends AngularPluginBase<
        t.TypeOf<typeof multisaveMarkup>,
        t.TypeOf<typeof multisaveAll>,
        typeof multisaveAll
    >
    implements IChangeListener
{
    isSaved = false;
    vctrl!: ViewCtrl;
    savedFields: number = 0;
    private unsavedTimComps: Set<string> = new Set<string>();
    private hasUnsavedTargets: boolean = false;
    unsavedTasksWithAliases: {component: ITimComponent; alias?: string}[] = [];
    requiresTaskId = false;
    listener = false;
    timer?: number;
    saveMessages: string[] = [];

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

    buttonText() {
        return super.buttonText() ?? $localize`Save`;
    }

    get allSavedText() {
        return this.markup.allSavedText;
    }

    get unsavedText() {
        return this.markup.unsavedText?.replace(
            "{count}",
            this.unsavedTasksWithAliases.length.toString()
        );
    }

    get savedText() {
        return this.markup.savedText ?? $localize`Saved`;
    }

    get livefeed() {
        return this.markup.livefeed;
    }

    /**
     * Return alias or name for an unsaved task
     * Preference:
     * alias by docid.taskname -> alias by taskname -> getName -> undefined
     * @param task ITimComponent task to look up
     */
    getUnsavedAlias(task: ITimComponent): string | undefined {
        if (this.markup.aliases) {
            const tid = task.getTaskId();
            if (tid) {
                const alias =
                    this.markup.aliases[tid.docTask().toString()] ??
                    this.markup.aliases[tid.name];
                if (alias) {
                    return alias;
                }
            }
        }
        return task.getName();
    }

    ngOnInit() {
        super.ngOnInit();
        if (
            (this.markup.disableUnchanged ||
                this.markup.livefeed ||
                this.markup.undo) &&
            this.vctrl
        ) {
            this.listener = true;
            this.vctrl.addChangeListener(this);
        }
    }

    findTargetTasks(): ITimComponent[] {
        let targets: ITimComponent[] = [];
        if (this.listener) {
            for (const unsaved of this.unsavedTimComps) {
                const target = this.vctrl.getTimComponentByName(unsaved);
                if (target) {
                    targets.push(target);
                }
            }
            return targets;
        }
        // TODO: get components from vctrl.timComponentArrays in case of duplicates
        if (this.markup.fields) {
            for (const i of this.markup.fields) {
                const timComponents = this.vctrl.getTimComponentsByRegex(
                    i,
                    RegexOption.PrependCurrentDocId
                );
                for (const v of timComponents) {
                    if (!targets.includes(v)) {
                        targets.push(v);
                    }
                }
            }
        }

        if (this.markup.areas) {
            for (const i of this.markup.areas) {
                const timComponents = this.vctrl.getTimComponentsByArea(i);
                for (const v of timComponents) {
                    if (!targets.includes(v)) {
                        targets.push(v);
                    }
                }
            }
        }
        if (this.markup.tags) {
            for (const i of this.markup.tags) {
                const timComponents = this.vctrl.getTimComponentsByTag(i);
                for (const v of timComponents) {
                    if (!targets.includes(v)) {
                        targets.push(v);
                    }
                }
            }
        }

        let ownArea: string | undefined;
        const parents = this.element.parents(".area");
        // parents returns only one element because nested areas are in separate divs
        if (parents[0]) {
            ownArea = parents[0].classList[
                parents[0].classList.length - 1
            ].replace("area_", "");
        }

        // no given followids or areas but the plugin is inside an area
        if (
            !this.markup.fields &&
            !this.markup.areas &&
            !this.markup.tags &&
            ownArea
        ) {
            targets = this.vctrl.getTimComponentsByArea(ownArea);
        }

        // no given followids / areas and no own area found
        if (
            !this.markup.fields &&
            !this.markup.areas &&
            !this.markup.tags &&
            !ownArea
        ) {
            targets = this.vctrl.getAllTimComponents();
        }
        return targets;
    }

    /**
     * Calls the save method of all ITimComponent plugins that match the given attributes
     * - Save all plugins defined in "fields" attribute that match the given regexp
     * - Save all plugins that are in the areas defined by "areas" attribute
     * - If fields/areas are not given then save only plugins in the same area with the multisave plugin
     * - If fields/areas are not given and multisave is not within any areas then just call save for every ITimComponent
     *   plugin in the same document
     */
    async save() {
        const componentsToSave = this.findTargetTasks();

        const promises = [];
        for (const v of componentsToSave) {
            if (this.markup.saveDelay) {
                await timeout(this.markup.saveDelay);
            }
            const result = v.save();
            promises.push(result);
        }

        this.isSaved = false;
        this.savedFields = 0;
        let savedIndex = 0;
        const fieldsToUpdate: string[] = [];
        this.saveMessages = [];
        for (const p of promises) {
            const result = await p;
            if (result.saved) {
                this.savedFields++;
                const tid = componentsToSave[savedIndex].getTaskId();
                if (tid) {
                    fieldsToUpdate.push(tid.docTask().toString());
                }
            }
            if (this.markup.showMessages && result.message) {
                let msg = result.message;
                if (this.markup.messageOverrides) {
                    for (const o of this.markup.messageOverrides) {
                        const reg = new RegExp(`^${o.expect}$`);
                        if (reg.test(msg)) {
                            msg = o.replace;
                        }
                    }
                }
                if (!this.saveMessages.includes(msg)) {
                    this.saveMessages.push(msg);
                }
            }
            savedIndex++;
        }
        if (this.markup.timer && !this.allSaved()) {
            this.setTimer();
        }
        if (this.markup.autoUpdateTables) {
            this.vctrl.updateAllTables(fieldsToUpdate);
        }
        if (this.markup.autoUpdateDuplicates) {
            const duplicatedFieldsToUpdate = [];
            for (const field of fieldsToUpdate) {
                const duplicates = this.vctrl.getTimComponentArray(field);
                if (duplicates && duplicates.length > 1) {
                    duplicatedFieldsToUpdate.push(field);
                }
            }
            if (duplicatedFieldsToUpdate.length > 0) {
                await this.vctrl.updateFields(duplicatedFieldsToUpdate);
            }
        }
        if (this.savedFields !== 0) {
            this.isSaved = true;
        }

        if (this.markup.jumplink) {
            // If there is need for jumplink
            const values = [];
            for (const v of componentsToSave) {
                const value = v.getContent();
                values.push(value);
            }

            let link = this.markup.jumplink;
            for (let i = 0; i < values.length; i++) {
                const val = values[i] ?? "";
                link = link.replace(`{${i}}`, encodeURIComponent(val));
                link = link.replace(
                    `{${i}:slug}`,
                    encodeURIComponent(slugify(val))
                );
            }
            const target = this.markup.jumptarget ?? "_self";
            window.open(link, target);
        }
    }

    public getTags(): string[] | undefined {
        return this.markup.tags;
    }

    private refreshUnsavedList() {
        const newUnsaved: typeof this.unsavedTasksWithAliases = [];
        for (const name of this.unsavedTimComps) {
            const c = this.vctrl.getTimComponentByName(name);
            if (c) {
                const alias = this.getUnsavedAlias(c);
                if (!alias || !newUnsaved.find((r) => r.alias === alias)) {
                    newUnsaved.push({component: c, alias});
                }
            }
        }
        this.unsavedTasksWithAliases = newUnsaved;
    }

    private addNewUnsaved(taskId: string) {
        this.unsavedTimComps.add(taskId);
        this.hasUnsavedTargets = true;
        this.refreshUnsavedList();
        this.isSaved = false;
        if (this.markup.timer) {
            if (this.timer) {
                window.clearTimeout(this.timer);
            }
            this.setTimer();
        }
    }

    private setTimer() {
        if (!this.markup.timer) {
            return;
        }
        if (this.timer) {
            window.clearTimeout(this.timer);
        }
        this.timer = window.setTimeout(() => this.save(), this.markup.timer);
    }

    public informAboutChanges(taskId: TaskId, state: ChangeType, tag?: string) {
        this.zone.run(() => {
            this.doInform(taskId, state, tag);
        });
    }

    private doInform(taskId: TaskId, state: ChangeType, tag?: string) {
        const docTask = taskId.docTask().toString();
        if (state == ChangeType.Saved) {
            if (this.unsavedTimComps.delete(docTask)) {
                if (this.unsavedTimComps.size == 0) {
                    this.hasUnsavedTargets = false;
                }
                this.refreshUnsavedList();
            }
            return;
        }
        // TODO: check here if taskId already in this.unsavedTimComps to ignore input spam?
        if (this.markup.tags && tag && this.markup.tags.includes(tag)) {
            this.addNewUnsaved(docTask);
            return;
        }
        if (this.markup.fields) {
            let reg: RegExp;
            for (const f of this.markup.fields) {
                // TODO: Handle fields from other docs pasted as reference
                reg = new RegExp(
                    `^${this.vctrl.docId + escapeRegExp(".") + f}$`
                );
                if (reg.test(docTask)) {
                    this.addNewUnsaved(docTask);
                    return;
                }
            }
        }
        if (!this.markup.areas && !this.markup.fields && !this.markup.tags) {
            // TODO: Check if task in this.areas or in multisave's own area?
            this.addNewUnsaved(docTask);
        }
    }

    allSaved(): boolean {
        return !this.hasUnsavedTargets;
    }

    getAttributeType() {
        return multisaveAll;
    }

    async tryResetChanges(e?: Event) {
        if (this.undoConfirmation) {
            if (
                !(await showConfirm(
                    this.undoConfirmationTitle ?? this.undoConfirmation,
                    this.undoConfirmation
                ))
            ) {
                return;
            }
        }
        const targets = this.findTargetTasks();
        for (const target of targets) {
            if (target) {
                target.resetChanges();
            }
        }
    }

    scrollTo(target: ITimComponent) {
        scrollToElement(target.getPar()?.getContent());
    }
}

@NgModule({
    declarations: [MultisaveComponent],
    imports: [
        CommonModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        SisuAssessmentExportModule,
        PurifyModule,
    ],
})
export class MultisaveModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

registerPlugin("tim-multisave", MultisaveModule, MultisaveComponent);
