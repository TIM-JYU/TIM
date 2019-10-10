/**
 * Defines the client-side implementation of a plugin that calls other plugins' save methods.
 */
import angular, {INgModelOptions} from "angular";
import * as t from "io-ts";
import {ITimComponent, RegexOption, ViewCtrl} from "tim/document/viewctrl";
import {GenericPluginMarkup, Info, withDefault} from "tim/plugin/attributes";
import {PluginBase, pluginBindings} from "tim/plugin/util";
import {showMessageDialog} from "tim/ui/dialog";
import {Users} from "tim/user/userService";
import {$http} from "tim/util/ngimport";
import {to} from "tim/util/utils";

const multisaveApp = angular.module("multisaveApp", ["ngSanitize"]);
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
        jumplink: t.string,
        jumptarget: t.string,
        destCourse: t.string,
    }),
    GenericPluginMarkup,
    t.type({
        // all withDefaults should come here; NOT in t.partial
        emailMode: withDefault(t.boolean, false),
        autoUpdateDuplicates: withDefault(t.boolean, true),
        autoUpdateTables: withDefault(t.boolean, true),
    }),
]);
const multisaveAll = t.intersection([
    t.partial({
    }),
    t.type({
        info: Info,
        markup: multisaveMarkup,
        preview: t.boolean,
    }),
]);

interface IAssessment {
    completionCredits?: number;
    completionDate: string;
    gradeId: "1" | "2" | "3" | "4" | "5" | "HYV" | "HYL" | "HT" | "TT";
    privateComment?: string;
    userName: string;
}

interface IAssessmentError {
    message: string;
    assessment: IAssessment;
}

interface IAssessmentExt extends IAssessment {
    error?: string;
}

interface IGradeResponse {
    sent_assessments: IAssessment[];
    assessment_errors: IAssessmentError[];
}

export class MultisaveController extends PluginBase<t.TypeOf<typeof multisaveMarkup>, t.TypeOf<typeof multisaveAll>, typeof multisaveAll> {
    private isSaved = false;
    private modelOpts!: INgModelOptions; // initialized in $onInit, so need to assure TypeScript with "!"
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
    private assessments?: IAssessmentExt[];
    private gridOptions?: uiGrid.IGridOptionsOf<IAssessmentExt>;
    private partial: boolean = false;
    private dryRun: boolean = false;
    private saving: boolean = false;
    private lastPartial?: boolean;
    private lastDryRun?: boolean;

    getDefaultMarkup() {
        return {};
    }

    buttonText() {
        return super.buttonText() || (this.attrs.emailMode && "Send email") || "Save";
    }

    $onInit() {
        super.$onInit();
        if (this.attrs.emailRecipients) {
            this.emaillist = this.attrs.emailRecipients.join("\n");
        }
        this.emailbody = this.attrs.emailPreMsg;
        this.emailsubject = this.attrs.emailSubject;
    }

    async sendEmailTim() {
        if (!this.emaillist || !this.emailsubject) {
            return;
        }
        this.emailMsg = ""; // JSON.stringify(response);

        const response = await $http.post<string[]>("/sendemail/", {
            rcpts: this.emaillist.replace(/\n/g, ";"),
            subject: this.emailsubject,
            msg: this.emailbody,
            bccme: this.emailbccme,
        });

       /* const url = this.pluginMeta.getAnswerUrl()
            .replace("tableForm", "multiSendEmail")
            .replace("/answer", "");
        const response = await $http.post<string[]>(url, {
            // rcpt: this.emaillist.replace(/\n/g, ";"),
            rcpt: this.emaillist,
            subject: this.emailsubject,
            msg: this.emailbody,
            bccme: this.emailbccme,
        });
        this.emailMsg = "Sent"; // JSON.stringify(response);
        */

        return;
    }

    /**
     * TODO: Generic - move and import
     */
    public async sendEmail() {
        if (!this.emaillist || !this.emailsubject) {
            return;
        }
        if ( this.emailtim ) {
            this.sendEmailTim();
            return;
        }
        // TODO: iPad do not like ;
        let  addrs = this.emaillist.replace(/\n/g, ",");
        let bcc = "";
        if ( this.emailbcc ) {
            bcc = addrs;
            addrs = "";
        }
        if ( this.emailbccme ) {
            if ( bcc ) { bcc += ","; }
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

    okAssessments() {
        if (!this.assessments) {
            return 0;
        }
        return this.assessments.reduce((n, x) => n + (x.error === undefined ? 1 : 0), 0);
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
        if (this.attrs.destCourse) {
            this.saving = true;
            this.lastPartial = this.partial;
            this.lastDryRun = this.dryRun;
            this.assessments = undefined;
            const r = await to($http.post<IGradeResponse>("/sisu/sendGrades", {
                destCourse: this.attrs.destCourse,
                docId: this.vctrl.item.id,
                partial: this.partial,
                dryRun: this.dryRun,
            }));
            this.saving = false;
            if (r.ok) {
                this.assessments = [
                    ...r.result.data.sent_assessments,
                    ...r.result.data.assessment_errors.map((a) => ({...a.assessment, error: a.message})),
                ];
                this.gridOptions = {
                    columnDefs: [
                        {
                            field: "userName",
                            name: "Username",
                            allowCellFocus: false,
                            width: 140,
                        },
                        {
                            field: "gradeId",
                            name: "Grade",
                            allowCellFocus: false,
                            width: 60,
                        },
                        {
                            field: "completionDate",
                            name: "Completion date",
                            allowCellFocus: false,
                            width: 140,
                        },
                        {
                            field: "completionCredits",
                            name: "Credits",
                            allowCellFocus: false,
                            width: 70,
                        },
                        {
                            field: "privateComment",
                            name: "Comment",
                            allowCellFocus: false,
                            cellTooltip: true,
                        },
                        {
                            field: "error",
                            name: "Error?",
                            allowCellFocus: false,
                            sort: {direction: "asc"},
                            cellTooltip: true,
                        },
                    ],
                    data: this.assessments,
                    enableHorizontalScrollbar: false,
                    enableSorting: true,
                    enableRowSelection: false,
                    enableFiltering: true,
                    enableColumnMenus: false,
                };
            } else {
                await showMessageDialog(r.result.data.error);
            }
            return;
        }

        if (this.attrs.emailMode) {
            this.toggleEmailForm();
            return;
        }
        let componentsToSave: ITimComponent[] = [];
        // TODO: get components from vctrl.timComponentArrays in case of duplicates
        if (this.attrs.fields) {
            for (const i of this.attrs.fields) {
                const timComponents = this.vctrl.getTimComponentsByRegex(i, RegexOption.PrependCurrentDocId);
                for (const v of timComponents) {
                    if (!componentsToSave.includes(v)) { componentsToSave.push(v); }
                }
            }
        }

        if (this.attrs.areas) {
            for (const i of this.attrs.areas) {
                const timComponents = this.vctrl.getTimComponentsByArea(i);
                for (const v of timComponents) {
                    if (!componentsToSave.includes(v)) { componentsToSave.push(v); }
                }
            }
        }
        if (this.attrs.tags) {
            for (const i of this.attrs.tags) {
                const timComponents = this.vctrl.getTimComponentsByTag(i);
                for (const v of timComponents) {
                    if (!componentsToSave.includes(v)) {
                        componentsToSave.push(v);
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
            componentsToSave = this.vctrl.getTimComponentsByArea(ownArea);
        }

        // no given followids / areas and no own area found
        if (!this.attrs.fields && !this.attrs.areas && !this.attrs.tags && !ownArea) {
            componentsToSave = this.vctrl.getTimComponentsByRegex(".*", RegexOption.DontPrependCurrentDocId);
        }

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

        if ( this.attrs.jumplink ) { // If there is need for jumplink
            const values = [];
            for (const v of componentsToSave) {
                const value = v.getContent();
                values.push(value);
            }

            let link = this.attrs.jumplink;
            for (let i = 0; i < values.length; i++) {
                link = link.replace("{" + i + "}", values[i] || "");
            }
            const target = this.attrs.jumptarget || "_self";
            window.open(link, target);
        }
    }

    protected getAttributeType() {
        return multisaveAll;
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
    <tim-markup-error ng-if="::$ctrl.markupError" data="::$ctrl.markupError"></tim-markup-error>
    <h4 ng-if="::$ctrl.header" ng-bind-html="::$ctrl.header"></h4>
    <div ng-if="$ctrl.attrs.destCourse">
        <div class="checkbox">
            <label><input type="checkbox" ng-model="$ctrl.dryRun">
                Only see what would be sent
            </label>
        </div>
        <div class="checkbox">
            <label><input type="checkbox" ng-model="$ctrl.partial">
                If some assessments have errors, send still the ones that are ok
            </label>
        </div>
    </div>
    <button class="timButton"
            ng-disabled="$ctrl.saving"
            ng-if="!$ctrl.showEmailForm && $ctrl.buttonText()"
            ng-click="$ctrl.save()">
        {{::$ctrl.buttonText()}}
    </button>
    <div ng-if="$ctrl.assessments">
        <p>
            {{ $ctrl.okAssessments() }} assessments {{ $ctrl.lastDryRun ? 'would be' : 'were' }} sent to Sisu.
            {{ $ctrl.assessments.length - $ctrl.okAssessments() }} assessments with errors {{ $ctrl.lastDryRun ? 'would be' : 'were' }} rejected.
        </p>
        <div style="font-size: small"
             ui-grid="$ctrl.gridOptions"
             ui-grid-auto-resize
             ui-grid-cellNav>
        </div>
    </div>
    <p class="savedtext" ng-if="$ctrl.isSaved">Saved {{$ctrl.savedFields}} fields!</p>
    <div class="csRunDiv multisaveEmail" style="padding: 1em;" ng-if="$ctrl.showEmailForm"> <!-- email -->
        <p class="closeButton" ng-click="$ctrl.toggleEmailForm()"></p>
        <p><textarea id="emaillist" ng-model="$ctrl.emaillist" rows="4" cols="40"></textarea><p>
        <p>
        <label title="Send so that names are not visible (works only non-TIM send)"><input type="checkbox" ng-model="$ctrl.emailbcc">BCC</label>&nbsp;
        <label title="Send also a copy for me"><input type="checkbox" ng-model="$ctrl.emailbccme" >BCC also for me</label>&nbsp;
        <label title="Send using TIM. Every mail is sent as a personal mail."><input type="checkbox" ng-model="$ctrl.emailtim" >use TIM to send</label>&nbsp;
        </p>
        <p>Subject: <input ng-model="$ctrl.emailsubject" size="60"></p>
        <p>eMail content:</p>
        <p><textarea id="emaillist" ng-model="$ctrl.emailbody" rows="10" cols="70"></textarea></p>
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
