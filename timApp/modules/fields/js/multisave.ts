/**
 * Defines the client-side implementation of a plugin that calls other plugins' save methods.
 */
import angular from "angular";
import * as t from "io-ts";
import {ChangeType, IChangeListener, ITimComponent, RegexOption, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, IncludeUsersOption, Info, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {Users} from "tim/user/userService";
import {$http} from "tim/util/ngimport";
import {escapeRegExp, to} from "tim/util/utils";
import {TaskId} from "tim/plugin/taskid";
import {GroupType, Sisu} from "./sisuassessmentexport";

const multisaveApp = angular.module("multisaveApp", ["ngSanitize", Sisu.name]);
export const moduleDefs = [multisaveApp];

const multisaveMarkup = t.intersection([
    t.partial({
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
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        emailMode: withDefault(t.boolean, false),
        autoUpdateDuplicates: withDefault(t.boolean, true),
        autoUpdateTables: withDefault(t.boolean, true),
        listener: withDefault(t.boolean, false),
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

export class MultisaveController
    extends PluginBase<t.TypeOf<typeof multisaveMarkup>, t.TypeOf<typeof multisaveAll>, typeof multisaveAll>
    implements IChangeListener {
    private isSaved = false;
    private vctrl!: ViewCtrl;
    private savedFields: number = 0;
    private showEmailForm: boolean = false;
    private emaillist: string | undefined = "";
    private emailsubject: string | undefined = "";
    private emailbody: string | undefined = "";
    private emailbcc: boolean = false;
    private emailbccme: boolean = true;
    private emailtim: boolean = true;
    private emailMsg: string = "";
    private unsavedTimComps: Set<string> = new Set<string>();
    private hasUnsavedTargets: boolean = false;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || (this.attrs.emailMode && "Send email") || "Save";
    }

    get listener() {
        return this.attrs.listener;
    }

    $onInit() {
        super.$onInit();
        if (this.attrs.emailRecipients) {
            this.emaillist = this.attrs.emailRecipients.join("\n");
        }
        if (this.attrs.listener && this.vctrl) {
            this.vctrl.addChangeListener(this);
        }
        this.emailbody = this.attrs.emailPreMsg;
        this.emailsubject = this.attrs.emailSubject;
    }

    async sendEmailTim() {
        if (!this.emaillist || !this.emailsubject) {
            return;
        }
        this.emailMsg = "";

        const response = await to($http.post<string[]>("/sendemail/", {
            rcpts: this.emaillist.replace(/\n/g, ";"),
            subject: this.emailsubject,
            msg: this.emailbody,
            bccme: this.emailbccme,
        }));
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
        window.location.href = "mailto:" + addrs
            + "?" + "subject=" + this.emailsubject
            + "&" + "body=" + this.emailbody
            + "&" + "bcc=" + bcc;
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
        // TODO: get components from vctrl.timComponentArrays in case of duplicates
        if (this.attrs.fields) {
            for (const i of this.attrs.fields) {
                const timComponents = this.vctrl.getTimComponentsByRegex(i, RegexOption.PrependCurrentDocId);
                for (const v of timComponents) {
                    if (!targets.includes(v)) {
                        targets.push(v);
                    }
                }
            }
        }

        if (this.attrs.areas) {
            for (const i of this.attrs.areas) {
                const timComponents = this.vctrl.getTimComponentsByArea(i);
                for (const v of timComponents) {
                    if (!targets.includes(v)) {
                        targets.push(v);
                    }
                }
            }
        }
        if (this.attrs.tags) {
            for (const i of this.attrs.tags) {
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
            ownArea = parents[0].classList[parents[0].classList.length - 1].replace("area_", "");
        }

        // no given followids or areas but the plugin is inside an area
        if (!this.attrs.fields && !this.attrs.areas && !this.attrs.tags && ownArea) {
            targets = this.vctrl.getTimComponentsByArea(ownArea);
        }

        // no given followids / areas and no own area found
        if (!this.attrs.fields && !this.attrs.areas && !this.attrs.tags && !ownArea) {
            targets = this.vctrl.getTimComponentsByRegex(".*", RegexOption.DontPrependCurrentDocId);
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
        if (this.attrs.emailMode) {
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
                    fieldsToUpdate.push(tid.docTask());
                }
            }
            savedIndex++;
        }
        if (this.attrs.autoUpdateTables) {
            this.vctrl.updateAllTables(fieldsToUpdate);
        }
        if (this.attrs.autoUpdateDuplicates) {
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

        if (this.attrs.jumplink) { // If there is need for jumplink
            const values = [];
            for (const v of componentsToSave) {
                const value = v.getContent();
                values.push(value);
            }

            let link = this.attrs.jumplink;
            for (let i = 0; i < values.length; i++) {
                link = link.replace("{" + i + "}", values[i] ?? "");
            }
            const target = this.attrs.jumptarget ?? "_self";
            window.open(link, target);
        }
    }

    public getTags(): string[] | undefined {
        return this.attrs.tags;
    }

    private addNewUnsaved(taskId: string) {
        this.unsavedTimComps.add(taskId);
        this.hasUnsavedTargets = true;
        this.isSaved = false;
    }

    public informAboutChanges(taskId: TaskId, state: ChangeType, tag?: string) {
        if (!this.attrs.listener) {
            return;
        }
        const docTask = taskId.docTask();
        if (state == ChangeType.Saved) {
            if (this.unsavedTimComps.delete(docTask)) {
                if (this.unsavedTimComps.size == 0) {
                    this.hasUnsavedTargets = false;
                }
            }
            return;
        }
        // TODO: check here if taskId already in this.unsavedTimComps to ignore input spam?
        if (this.attrs.tags && tag && this.attrs.tags.includes(tag)) {
            this.addNewUnsaved(docTask);
            return;
        }
        if (this.attrs.fields) {
            let reg: RegExp;
            for (const f of this.attrs.fields) {
                // TODO: Handle fields from other docs pasted as reference
                reg = new RegExp(`^${this.vctrl.docId + escapeRegExp(".") + f}$`);
                if (reg.test(docTask)) {
                    this.addNewUnsaved(docTask);
                    return;
                }
            }
        }
        if (!this.attrs.areas && !this.attrs.fields && !this.attrs.tags) {
            // TODO: Check if task in this.areas or in multisave's own area?
            this.addNewUnsaved(docTask);
        }
    }

    allSaved(): boolean {
        return (this.attrs.listener && !this.hasUnsavedTargets);
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
            target.resetChanges();
        }
    }
}

multisaveApp.component("multisaveRunner", {
    bindings: pluginBindings,
    controller: MultisaveController,
    require: {
        vctrl: "^timView",
    },
    template: `
<span class="no-popup-menu">
    <tim-markup-error ng-if="::$ctrl.markupError" [data]="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <sisu-assessment-export ng-if="$ctrl.attrs.destCourse"
                            doc-id="$ctrl.vctrl.item.id"
                            dest-course="$ctrl.attrs.destCourse"
                            include-users="$ctrl.attrs.includeUsers"
                            test-only="$ctrl.attrs.testOnly"
                            group="$ctrl.attrs.group">
    </sisu-assessment-export>
    <button class="timButton"
            ng-disabled="($ctrl.disableUnchanged && $ctrl.allSaved())"
            ng-if="!$ctrl.showEmailForm && $ctrl.buttonText() && !$ctrl.attrs.destCourse"
            ng-click="$ctrl.save()">
        {{::$ctrl.buttonText()}}
    </button>
    &nbsp;
    <a href="" ng-if="($ctrl.listener && !$ctrl.allSaved())" title="{{::$ctrl.undoTitle}}" ng-click="$ctrl.resetChanges();">{{::$ctrl.undoButton}}</a>
    <p class="savedtext" ng-if="$ctrl.isSaved">Saved {{$ctrl.savedFields}} fields!</p>
    <div class="csRunDiv multisaveEmail" style="padding: 1em;" ng-if="$ctrl.showEmailForm"> <!-- email -->
        <tim-close-button ng-click="$ctrl.toggleEmailForm()"></tim-close-button>
        <p><textarea ng-model="$ctrl.emaillist" rows="4" cols="40"></textarea>
        <p>
        <p>
            <label title="Send so that names are not visible (works only non-TIM send)"><input type="checkbox"
                                                                                               ng-model="$ctrl.emailbcc">BCC</label>&nbsp;
            <label title="Send also a copy for me"><input type="checkbox"
                                                          ng-model="$ctrl.emailbccme">BCC also for me</label>&nbsp;
            <label title="Send using TIM. Every mail is sent as a personal mail."><input type="checkbox"
                                                                                         ng-model="$ctrl.emailtim">use
                TIM to send</label>&nbsp;
        </p>
        <p>Subject: <input ng-model="$ctrl.emailsubject" size="60"></p>
        <p>eMail content:</p>
        <p><textarea ng-model="$ctrl.emailbody" rows="10" cols="70"></textarea></p>
        <p>
            <button class="timButton"
                    ng-click="$ctrl.sendEmail()">
                Send
            </button>
            <span class="savedtext" ng-if="$ctrl.emailMsg">Sent!</span>
        </p>
    </div>
    <p ng-if="::$ctrl.footer" ng-bind="::$ctrl.footer" class="plgfooter"></p>
</span>
`,
});
