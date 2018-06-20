/***
 * Dialog for tagging course meta data including course code and subject.
 */

import {IFormController, IRootElementService, IScope} from "angular";
import {DialogController, registerDialogComponent, showDialog} from "../dialog";
import {IItem, ITag, TagType} from "../IItem";
import {to} from "../utils";
import {$http} from "../ngimport";
import {IParResponse} from "../edittypes";
import {Moment} from "moment";

/*
 * Tag search dialog's controller.
 */
export class ShowCourseDialogController extends DialogController<{ params: IItem }, {}, "timCourseDialog"> {
    private static $inject = ["$element", "$scope"];
    private f: IFormController;
    private courseSubject: string;
    private courseCode: string;
    private expires: Moment;
    private errorMessage: string;
    private successMessage: string;

    // TODO: Get this from a TIM page.
    private subjects = ["matematiikka", "fysiikka", "psykologia", "tilastotiede"];

    constructor(protected element: IRootElementService, protected scope: IScope) {
        super(element, scope);
    }

    /*
     * Show tag list when dialog loads and focus on tag-field.
     */
    async $onInit() {
        super.$onInit();
    }

    /*
     * Dialog title.
     */
    public getTitle() {
        return "Set as a course";
    }

    /***
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

        const codeTag = {
            block_id: this.resolve.params.id, expires: this.expires,
            name: this.courseCode.trim(), type: TagType.CourseCode,
        };
        const subjectTag = {
            block_id: this.resolve.params.id, expires: this.expires,
            name: this.courseSubject.trim(), type: TagType.Subject,
        };
        const data = {tags: [codeTag, subjectTag]};
        const [err, response] = await to($http.post<IParResponse>(`/tags/add/${docPath}`, data));

        if (err) {
            this.errorMessage = err.data.error;
            this.successMessage = "";
            return;
        }
        if (response) {
            this.successMessage = "'" + this.courseCode + "' successfully added as a '"
                    + this.courseSubject + "' course.";
            return;
        }
    }
}

registerDialogComponent("timCourseDialog",
    ShowCourseDialogController,
    {
        template:
            `<tim-dialog>
    <dialog-header>
    </dialog-header>
    <dialog-body>
        <p>Tag document as a course main page by sending its course code and subject.</p>
        <form name="$ctrl.f" class="form-horizontal">
            <div class="form-group" tim-error-state>
                <label for="codeField" class="col-sm-4 control-label">Course code:</label>
                <div class="col-sm-8">
                    <input required focus-me="$ctrl.focusName" ng-model="$ctrl.courseCode" name="codeField"
                           type="text" title="" autocomplete="off"
                           class="form-control" id="codeField"
                           placeholder="Input the course code using capital letters">
                </div>
                <tim-error-message></tim-error-message>
            </div>

            <div class="form-group">
                <label for="courseSelect" class="col-sm-4 control-label">Subject:</label>
                <div class="col-sm-8">
                    <select required class="form-control" id="courseSelect" ng-model="$ctrl.courseSubject"
                        title="Select the subject of the course." name="courseSelect">
                        <option ng-repeat="subject in $ctrl.subjects">{{subject}}</option>
                    </select>
                </div>
            </div>
            <div class="form-group" title="Add optional expiration date to specify how long the course is valid">
                <label for="name" class="col-sm-4 control-label">Expiration date:</label>
                <div class="col-sm-8">
                    <div class="input-group date" datetimepicker ng-model="$ctrl.expires"
                         data-options="datePickerOptionsFrom">
                        <input type="text" class="form-control"
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
        <button class="btn timButton" data-ng-disabled="$ctrl.f.$invalid" ng-click="$ctrl.registerCourse()"
        title="Send course meta data">
            <span>Send</span>
        </button>
        <button class="btn timButton" ng-click="$ctrl.dismiss()"><span>Close</span></button>
    </dialog-footer>
</tim-dialog>
`,
    });

export async function showCourseDialog(d: IItem) {
    return await showDialog<ShowCourseDialogController>("timCourseDialog", {params: () => d}).result;
}
