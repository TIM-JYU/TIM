/**
 * Defines the client-side implementation of a plugin that calls other plugins' save methods.
 */
import * as t from "io-ts";
import {ApplicationRef, Component, DoBootstrap, NgModule} from "@angular/core";
import {
    ChangeType,
    IChangeListener,
    ITimComponent,
    RegexOption,
    ViewCtrl,
} from "tim/document/viewctrl";
import {
    GenericPluginMarkup,
    IncludeUsersOption,
    Info,
    withDefault,
} from "tim/plugin/attributes";
import {Users} from "tim/user/userService";
import {$http} from "tim/util/ngimport";
import {escapeRegExp, scrollToElement, to} from "tim/util/utils";
import {TaskId} from "tim/plugin/taskid";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {createDowngradedModule, doDowngrade} from "tim/downgrade";
import {platformBrowserDynamic} from "@angular/platform-browser-dynamic";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {vctrlInstance} from "tim/document/viewctrlinstance";
import {
    GroupType,
    SisuAssessmentExportModule,
} from "./sisu-assessment-export.component";

const multisaveMarkup = t.intersection([
    t.partial({
        allSavedText: t.string,
        areas: t.array(t.string),
        tags: t.array(t.string),
        emailPreMsg: t.string,
        emailRecipients: t.array(t.string),
        emailSubject: t.string,
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
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        emailMode: withDefault(t.boolean, false),
        autoUpdateDuplicates: withDefault(t.boolean, true),
        autoUpdateTables: withDefault(t.boolean, true),
        nosave: withDefault(t.boolean, false),
        listener: withDefault(t.boolean, false),
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
                <li *ngFor="let tag of unsaveds">
                    <a href="" (click)="scrollTo(tag)">{{tag.getName()}}</a>
                </li>
            </ul>
        </div>
        <div *ngIf="allSaved()">
            {{allSavedText}}
        </div>
    </div> <!-- unsaved fields -->
    <div *ngIf="!livefeed || !allSaved()">
    <button class="timButton"
            [disabled]="(markup.disableUnchanged && listener && allSaved())"
            *ngIf="!showEmailForm && buttonText() && !markup.destCourse"
            (click)="save()">
        {{buttonText()}}
    </button>
    &nbsp;
    <a href="" *ngIf="(undoButton && (!listener || !allSaved()))" title="{{undoTitle}}"
       (click)="resetChanges();">{{undoButton}}</a>
    <p class="savedtext" *ngIf="isSaved && allSaved()">{{savedText}}</p>
    </div>
    <div class="csRunDiv multisaveEmail" style="padding: 1em;" *ngIf="showEmailForm"> <!-- email -->
        <tim-close-button (click)="toggleEmailForm()"></tim-close-button>
        <p><textarea [(ngModel)]="emaillist" rows="4" cols="40"></textarea>
        <p>
        <p>
            <label title="Send so that names are not visible (works only non-TIM send)">
                <input type="checkbox"
                       [(ngModel)]="emailbcc">BCC</label>&nbsp;
            <label title="Send also a copy for me">
                <input type="checkbox"
                       [(ngModel)]="emailbccme">BCC also for me</label>&nbsp;
            <label title="Send using TIM. Every mail is sent as a personal mail.">
                <input type="checkbox"
                       [(ngModel)]="emailtim">use
                TIM to send</label>&nbsp;
        </p>
        <p>Subject: <input [(ngModel)]="emailsubject" size="60"></p>
        <p>eMail content:</p>
        <p><textarea [(ngModel)]="emailbody" rows="10" cols="70"></textarea></p>
        <p>
            <button class="timButton"
                    (click)="sendEmail()">
                Send
            </button>
            <span class="savedtext" *ngIf="emailMsg">Sent!</span>
        </p>
    </div> <!-- email-->
    <p *ngIf="footer" [innerText]="footer" class="plgfooter"></p>
</span>
    `,
})
export class MultisaveComponent
    extends AngularPluginBase<
        t.TypeOf<typeof multisaveMarkup>,
        t.TypeOf<typeof multisaveAll>,
        typeof multisaveAll
    >
    implements IChangeListener {
    isSaved = false;
    vctrl!: ViewCtrl;
    savedFields: number = 0;
    showEmailForm: boolean = false;
    emaillist: string | undefined = "";
    emailsubject: string | undefined = "";
    emailbody: string | undefined = "";
    emailbcc: boolean = false;
    emailbccme: boolean = true;
    emailtim: boolean = true;
    emailMsg: string = "";
    private unsavedTimComps: Set<string> = new Set<string>();
    private hasUnsavedTargets: boolean = false;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return (
            super.buttonText() ||
            (this.markup.emailMode && "Send email") ||
            "Save"
        );
    }

    get allSavedText() {
        return this.markup.allSavedText;
    }

    get unsavedText() {
        return this.markup.unsavedText?.replace(
            "{count}",
            this.unsavedTimComps.size.toString()
        );
    }

    get savedText() {
        return this.markup.savedText ?? "Saved";
    }

    get listener() {
        return this.markup.listener;
    }

    get livefeed() {
        return this.markup.livefeed;
    }

    get unsaveds() {
        const ret = [];
        for (const name of this.unsavedTimComps) {
            const c = this.vctrl.getTimComponentByName(name);
            if (c) {
                ret.push(c);
            }
        }
        return ret;
    }

    ngOnInit() {
        super.ngOnInit();
        this.vctrl = vctrlInstance!;
        if (this.markup.emailRecipients) {
            this.emaillist = this.markup.emailRecipients.join("\n");
        }
        if (this.markup.listener && this.vctrl) {
            this.vctrl.addChangeListener(this);
        }
        this.emailbody = this.markup.emailPreMsg;
        this.emailsubject = this.markup.emailSubject;
    }

    async sendEmailTim() {
        if (!this.emaillist || !this.emailsubject) {
            return;
        }
        this.emailMsg = "";

        const _ = await to(
            $http.post<string[]>("/sendemail/", {
                rcpts: this.emaillist.replace(/\n/g, ";"),
                subject: this.emailsubject,
                msg: this.emailbody,
                bccme: this.emailbccme,
            })
        );
    }

    /**
     * TODO: Generic - move and import
     */
    public async sendEmail() {
        if (!this.emaillist || !this.emailsubject) {
            return;
        }
        if (this.emailtim) {
            await this.sendEmailTim();
            return;
        }
        // TODO: iPad do not like ;
        let addrs = this.emaillist.replace(/\n/g, ",");
        let bcc = "";
        if (this.emailbcc) {
            bcc = addrs;
            addrs = "";
        }
        if (this.emailbccme) {
            if (bcc) {
                bcc += ",";
            }
            bcc += Users.getCurrent().email;
        }
        window.location.href =
            "mailto:" +
            addrs +
            "?" +
            "subject=" +
            this.emailsubject +
            "&" +
            "body=" +
            this.emailbody +
            "&" +
            "bcc=" +
            bcc;
    }

    toggleEmailForm() {
        const tid = this.pluginMeta.getTaskId();
        // For now only tasks can send email
        if (!tid) {
            return;
        }
        this.showEmailForm = !this.showEmailForm;
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
            targets = this.vctrl.getTimComponentsByRegex(
                ".*",
                RegexOption.DontPrependCurrentDocId
            );
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
        if (this.markup.emailMode) {
            this.toggleEmailForm();
            return;
        }
        const componentsToSave = this.findTargetTasks();

        const promises = [];
        for (const v of componentsToSave) {
            const result = v.save();
            promises.push(result);
        }

        this.isSaved = false;
        this.savedFields = 0;
        let savedIndex = 0;
        const fieldsToUpdate: string[] = [];
        for (const p of promises) {
            const result = await p;
            if (result.saved) {
                this.savedFields++;
                const tid = componentsToSave[savedIndex].getTaskId();
                if (tid) {
                    fieldsToUpdate.push(tid.docTask().toString());
                }
            }
            savedIndex++;
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
                this.vctrl.updateFields(duplicatedFieldsToUpdate);
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
                link = link.replace("{" + i + "}", values[i] ?? "");
            }
            const target = this.markup.jumptarget ?? "_self";
            window.open(link, target);
        }
    }

    public getTags(): string[] | undefined {
        return this.markup.tags;
    }

    private addNewUnsaved(taskId: string) {
        this.unsavedTimComps.add(taskId);
        this.hasUnsavedTargets = true;
        this.isSaved = false;
    }

    public informAboutChanges(taskId: TaskId, state: ChangeType, tag?: string) {
        if (!this.markup.listener) {
            return;
        }
        // this.scope.$evalAsync(); // TODO
        const docTask = taskId.docTask().toString();
        if (state == ChangeType.Saved) {
            if (this.unsavedTimComps.delete(docTask)) {
                if (this.unsavedTimComps.size == 0) {
                    this.hasUnsavedTargets = false;
                }
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
        return !this.markup.listener || !this.hasUnsavedTargets;
    }

    getAttributeType() {
        return multisaveAll;
    }

    resetChanges() {
        if (this.undoConfirmation && !window.confirm(this.undoConfirmation)) {
            return;
        }
        const targets = this.findTargetTasks();
        for (const target of targets) {
            if (target) {
                target.resetChanges();
            }
        }
    }

    scrollTo(target: ITimComponent) {
        scrollToElement(target.getPar().children(".parContent")[0]);
    }
}

@NgModule({
    declarations: [MultisaveComponent],
    imports: [
        BrowserModule,
        HttpClientModule,
        FormsModule,
        TimUtilityModule,
        SisuAssessmentExportModule,
    ],
})
export class MultisaveModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}

export const moduleDefs = [
    doDowngrade(
        createDowngradedModule((extraProviders) =>
            platformBrowserDynamic(extraProviders).bootstrapModule(
                MultisaveModule
            )
        ),
        "timMultisave",
        MultisaveComponent
    ),
];
