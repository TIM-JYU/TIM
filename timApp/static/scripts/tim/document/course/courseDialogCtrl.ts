/**
 * Dialog for tagging course meta data including course code and subject.
 */

import {IFormController, IScope} from "angular";
import {Moment} from "moment";
import {DialogController} from "tim/ui/dialogController";
import {ICourseSettings, IItem, ISubjectList, ITag, TagType} from "../../item/IItem";
import {registerDialogComponent, showDialog} from "../../ui/dialog";
import {$http} from "../../util/ngimport";
import {dateFormat, to} from "../../util/utils";

const groupTagPrefix = "group:";

export class CourseDialogController extends DialogController<{params: IItem}, {}> {
    static component = "timCourseDialog";
    static $inject = ["$element", "$scope"] as const;
    private f!: IFormController; // initialized in the template
    private courseSubject: string = "";
    private courseCode: string = "";
    private expires: Moment | undefined;
    private errorMessage?: string;
    private focusName: boolean = true;
    private successMessage?: string;
    private subjects?: ISubjectList;
    private datePickerOptions: EonasdanBootstrapDatetimepicker.SetOptions;
    private currentCode: ITag | undefined;
    private currentSubject: ITag | undefined;
    private studentGroupName = "";

    constructor(protected element: JQLite, protected scope: IScope) {
        super(element, scope);
        this.datePickerOptions = {
            defaultDate: "",
            format: dateFormat,
            showTodayButton: true,
        };
    }

    // TODO: Support multiple subjects/codes.

    /**
     * Show tag list when dialog loads and focus on tag-field.
     */
    $onInit() {
        super.$onInit();

        (async () => {
            await this.getSubjects();
            await this.getCurrentSpecialTags();
            this.updateFields();
        })();
    }

    /**
     * Dialog title.
     */
    public getTitle() {
        return "Set as a course";
    }

    /**
     * Fetches the course special tags, if they exist.
     */
    private async getCurrentSpecialTags() {
        const docPath = this.resolve.params.path;
        const r = await to($http.get<ITag[]>(`/tags/getTags/${docPath}`));
        this.studentGroupName = "";
        let groupSep = "";
        if (r.ok) {
            const tags = r.result.data;
            for (const tag of tags) {
                if (tag.type === TagType.CourseCode) {
                    this.currentCode = tag;
                }
                if (tag.type === TagType.Subject) {
                    this.currentSubject = tag;
                }
                if (tag.name.startsWith(groupTagPrefix)) {
                    this.studentGroupName += groupSep + tag.name.slice(groupTagPrefix.length);
                    groupSep = ";";
                }
            }
        }
    }

    /**
     * Removes current code and subject tags so they can be replaced.
     */
    private async removeCurrentSpecialTags() {
        const docPath = this.resolve.params.path;
        const data = {tagObject: this.currentSubject};
        const r = await to($http.post(`/tags/setCourseTags/${docPath}`,
            {
                groups: [],
                tags: [],
            },
        ));
        if (!r.ok) {
            this.errorMessage = r.result.data.error;
            this.successMessage = undefined;
            return;
        }
        this.currentSubject = undefined;
        this.currentCode = undefined;
        this.studentGroupName = "";
    }

    /**
     * Displays current subject, code and expiration in the input fields.
     */
    private updateFields() {
        if (this.currentSubject && this.currentCode) {
            this.courseSubject = this.currentSubject.name;
            this.courseCode = this.currentCode.name;
            this.expires = this.currentSubject.expires;
        } else {
            this.courseSubject = "";
            this.courseCode = "";
            this.expires = undefined;
        }
    }

    /**
     * Removes course code and subject from the database.
     */
    private async unregisterCourse() {
        await this.removeCurrentSpecialTags();
        this.updateFields();
        this.errorMessage = undefined;
        this.successMessage = "Course code and subject successfully removed";
    }

    /**
     * Creates course special tags and adds them to database.
     */
    private async registerCourse() {
        if (this.f.$invalid) {
            return;
        }
        const docPath = this.resolve.params.path;
        if (!this.courseCode) {
            this.errorMessage = "Course code required!";
            this.successMessage = undefined;
            return;
        }
        if (!this.courseSubject) {
            this.errorMessage = "Course subject required!";
            this.successMessage = undefined;
            return;
        }

        const codeName = this.courseCode.trim().toUpperCase();
        const codeTag = {
            expires: this.expires, name: codeName, type: TagType.CourseCode,
        };
        const subjectTag = {
            expires: this.expires, name: this.courseSubject.trim(), type: TagType.Subject,
        };

        const r = await to($http.post(`/tags/setCourseTags/${docPath}`,
            {
                groups: this.studentGroupName.length > 0 ? this.studentGroupName.split(";") : [],
                tags: [codeTag, subjectTag],
            },
        ));
        if (!r.ok) {
            this.errorMessage = r.result.data.error;
            this.successMessage = undefined;
            return;
        }
        this.errorMessage = undefined;
        this.successMessage = `'${codeName}' successfully added as a '${this.courseSubject}' course.`;
        await this.getCurrentSpecialTags();
    }

    /**
     * Gets the subjects list from the designated course settings document.
     */
    private async getSubjects() {
        const r = await to($http.get<ICourseSettings>(`/courses/settings`));
        if (r.ok) {
            // Add a placeholder subject for quicker testing in case course settings does not exist.
            this.subjects = r.result.data.course_subjects ?? ["test"];
        } else {
            this.errorMessage = r.result.data.error;
            this.successMessage = undefined;
        }
    }
}

registerDialogComponent(CourseDialogController,
    {
        template:
            `<tim-dialog class="overflow-visible">
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <p>Tag document as a course main page by giving its course code and subject.</p>
        <form name="$ctrl.f" class="form-horizontal">
            <div class="form-group" tim-error-state>
                <label for="course-code-field" class="col-sm-4 control-label">Course code:</label>
                <div class="col-sm-8">
                    <input required focus-me="$ctrl.focusName" ng-model="$ctrl.courseCode" name="course-code-field"
                           type="text" title="" autocomplete="off"
                           class="form-control" id="course-code-field"
                           placeholder="Course code using capital letters">
                </div>
                <tim-error-message></tim-error-message>
            </div>

            <div class="form-group">
                <label for="course-selector" class="col-sm-4 control-label">Subject:
                <a href="/view/tim/kurssikategoriat"><span class="glyphicon glyphicon-question-sign"
          uib-tooltip="If a subject is missing from the list, click here to open the course category suggestion page.">
                </span>
                </a></label>
                <div class="col-sm-8">
                    <select required class="form-control" id="course-selector" ng-model="$ctrl.courseSubject"
                        title="Select the subject of the course." name="course-selector">
                        <option ng-repeat="subject in $ctrl.subjects | orderBy:subject">{{subject}}</option>
                    </select>
                </div>
            </div>
            <div class="form-group" title="Add optional expiration date to specify how long the course is valid">
                <label for="expiration-selector" class="col-sm-4 control-label">Expiration date:</label>
                <div class="col-sm-8">
                    <div class="input-group date" datetimepicker ng-model="$ctrl.expires"
                         data-options="$ctrl.datePickerOptions">
                        <input type="text" class="form-control" id="expiration-selector"
                               placeholder="Leave blank for indefinite period of validity"/>
                        <span class="input-group-addon">
                            <span class="glyphicon glyphicon-calendar"></span>
                        </span>
                    </div>
                </div>
            </div>

            <div class="form-group" tim-error-state>
                <label for="group-field" class="col-sm-4 control-label">Student group:</label>
                <div class="col-sm-8">
                    <input ng-model="$ctrl.studentGroupName" name="group-field"
                           type="text" autocomplete="off"
                           class="form-control" id="group-field"
                           placeholder="Course student group name">
                </div>
                <tim-error-message></tim-error-message>
            </div>
        </form>
        <div ng-show="$ctrl.successMessage" class="alert alert-success">
            <span class="glyphicon glyphicon-ok"></span> {{$ctrl.successMessage}}
        </div>
        <div ng-show="$ctrl.errorMessage" class="alert alert-warning">
            <span class="glyphicon glyphicon-exclamation-sign"></span> {{$ctrl.errorMessage}}
        </div>
    </dialog-body>
    <dialog-footer>
        <button class="timButton" data-ng-disabled="$ctrl.f.$invalid" ng-click="$ctrl.registerCourse()"
        title="Save course meta data">Set</button>
        <button class="timButton" data-ng-disabled="$ctrl.f.$invalid" ng-click="$ctrl.unregisterCourse()"
        title="Delete course meta data">Unset</button>
        <button class="timButton" ng-click="$ctrl.dismiss()" title="Leave without saving unsaved changes">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showCourseDialog(d: IItem) {
    return await showDialog(CourseDialogController, {params: () => d}).result;
}
