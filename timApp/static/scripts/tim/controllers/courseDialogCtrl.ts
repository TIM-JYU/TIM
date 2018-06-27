/**
 * Dialog for tagging course meta data including course code and subject.
 */

import {IFormController, IRootElementService, IScope} from "angular";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {ICourseSettings, IItem, ISubjectList, TagType} from "../IItem";
import {to} from "../utils";
import {$http} from "../ngimport";
import {Moment} from "moment";

export class ShowCourseDialogController extends DialogController<{ params: IItem }, {}, "timCourseDialog"> {
    private static $inject = ["$element", "$scope"];
    private f: IFormController;
    private courseSubject: string;
    private courseCode: string;
    private expires: Moment;
    private errorMessage: string;
    private successMessage: string;
    private subjects: ISubjectList;
    private datePickerOptions: EonasdanBootstrapDatetimepicker.SetOptions;

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    // TODO: Display and allow editing current code and subject in the dialog.
    // TODO: Support multiple subjects.

    /**
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        super.$onInit();
        await this.getSubjects();
        this.datePickerOptions = {
            format: "D.M.YYYY HH:mm:ss",
            defaultDate: "",
            showTodayButton: true,
        };
    }

    /**
     * Dialog title.
     */
    public getTitle() {
        return "Set as a course";
    }

    /**
     * Creates course special tags and adds them to database.
     * @returns {Promise<void>}
     */
    async registerCourse() {
        if (this.f.$invalid) {
            return;
        }
        const docPath = this.resolve.params.path;
        if (!this.courseCode) {
            this.errorMessage = "Course code required!";
            this.successMessage = "";
            return;
        }
        if (!this.courseSubject) {
            this.errorMessage = "Course subject required!";
            this.successMessage = "";
            return;
        }
        const codeName = this.courseCode.trim().toUpperCase();
        const codeTag = {
            expires: this.expires, name: codeName, type: TagType.CourseCode,
        };
        const subjectTag = {
            expires: this.expires,  name: this.courseSubject.trim(), type: TagType.Subject,
        };
        const data = {tags: [codeTag, subjectTag]};
        const [err, response] = await to($http.post(`/tags/add/${docPath}`, data));

        if (err) {
            this.errorMessage = err.data.error;
            this.successMessage = "";
            return;
        }
        if (response) {
            this.successMessage = `'${codeName}' successfully added as a '${this.courseSubject}' course.`;
            return;
        }
    }

    /**
     * Gets the subjects list from the designated course settings document.
     */
    private async getSubjects() {
        const [err, response] = await to($http.get<ICourseSettings>(`/courses/getCourseSettings`));
        if (response) {
            this.subjects = response.data.course_subjects;
        }
        if (err) {
            this.errorMessage = err.data.error;
            this.successMessage = "";
            return;
        }
    }
}

registerDialogComponent("timCourseDialog",
    ShowCourseDialogController,
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
                           placeholder="Input the course code using capital letters">
                </div>
                <tim-error-message></tim-error-message>
            </div>

            <div class="form-group">
                <label for="course-selector" class="col-sm-4 control-label">Subject:</label>
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
        title="Save course meta data">Save</button>
        <button class="timButton" ng-click="$ctrl.dismiss()">Close</button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showCourseDialog(d: IItem) {
    return await showDialog<ShowCourseDialogController>("timCourseDialog", {params: () => d}).result;
}
