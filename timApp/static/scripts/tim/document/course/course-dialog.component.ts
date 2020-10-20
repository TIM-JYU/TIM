/**
 * Dialog for tagging course meta data including course code and subject.
 */

import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule, ViewChild} from "@angular/core";
import {
    ICourseSettings,
    IItem,
    ISubjectList,
    ITag,
    TagType,
} from "tim/item/IItem";
import {to2} from "tim/util/utils";
import {FormsModule, NgForm} from "@angular/forms";
import {BrowserModule} from "@angular/platform-browser";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TooltipModule} from "ngx-bootstrap/tooltip";
import {DatetimePickerModule} from "tim/ui/datetime-picker/datetime-picker.component";
import {HttpClient} from "@angular/common/http";

const groupTagPrefix = "group:";

@Component({
    selector: "tim-course-dialog",
    template: `
        <tim-dialog-frame class="overflow-visible">
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <p>Tag document as a course main page by giving its course code and subject.</p>
                <form #f="ngForm" class="form-horizontal">
                    <div class="form-group" timErrorState>
                        <label for="course-code-field" class="col-sm-4 control-label">Course code:</label>
                        <div class="col-sm-8">
                            <input required focusMe [(ngModel)]="courseCode" name="course-code-field"
                                   type="text" title="" autocomplete="off"
                                   class="form-control" id="course-code-field"
                                   placeholder="Course code using capital letters">
                        </div>
                        <tim-error-message></tim-error-message>
                    </div>

                    <div class="form-group">
                        <label for="course-selector" class="col-sm-4 control-label">Subject:
                            <a href="/view/tim/kurssikategoriat">
                                <span class="glyphicon glyphicon-question-sign"
                                      tooltip="If a subject is missing from the list, click here to open the course category suggestion page.">
                </span>
                            </a></label>
                        <div class="col-sm-8">
                            <select required class="form-control" id="course-selector" [(ngModel)]="courseSubject"
                                    title="Select the subject of the course." name="course-selector">
                                <option *ngFor="let subject of subjects" [value]="subject">{{subject}}</option>
                            </select>
                        </div>
                    </div>
                    <div class="form-group"
                         title="Add optional expiration date to specify how long the course is valid">
                        <label for="expiration-selector" class="col-sm-4 control-label">Expiration date:</label>
                        <tim-datetime-picker id="expiration-selector"
                                             [(time)]="expires"
                                             placeholder="Leave blank for indefinite period of validity">
                        </tim-datetime-picker>
                    </div>

                    <div class="form-group" timErrorState>
                        <label for="group-field" class="col-sm-4 control-label">Student group:</label>
                        <div class="col-sm-8">
                            <input [(ngModel)]="studentGroupName" name="group-field"
                                   type="text" autocomplete="off"
                                   class="form-control" id="group-field"
                                   placeholder="Course student group name">
                        </div>
                        <tim-error-message></tim-error-message>
                    </div>
                </form>
                <div *ngIf="successMessage" class="alert alert-success">
                    <span class="glyphicon glyphicon-ok"></span> {{successMessage}}
                </div>
                <div *ngIf="errorMessage" class="alert alert-warning">
                    <span class="glyphicon glyphicon-exclamation-sign"></span> {{errorMessage}}
                </div>
            </ng-container>
            <ng-container footer>
                <button class="timButton" [disabled]="f.invalid" (click)="registerCourse()"
                        title="Save course meta data">Set
                </button>
                <button class="timButton" [disabled]="f.invalid" (click)="unregisterCourse()"
                        title="Delete course meta data">Unset
                </button>
                <button class="timButton" (click)="dismiss()" title="Leave without saving unsaved changes">Close
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class CourseDialogComponent extends AngularDialogComponent<IItem, void> {
    protected dialogName = "Course";
    @ViewChild("f")
    f!: NgForm;
    courseSubject: string = "";
    courseCode: string = "";
    expires: Date | undefined;
    errorMessage?: string;
    successMessage?: string;
    subjects?: ISubjectList;
    private currentCode: ITag | undefined;
    private currentSubject: ITag | undefined;
    studentGroupName = "";

    constructor(private http: HttpClient) {
        super();
    }

    // TODO: Support multiple subjects/codes.

    /**
     * Show tag list when dialog loads and focus on tag-field.
     */
    ngOnInit() {
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
        const docPath = this.data.path;
        const r = await to2(
            this.http.get<ITag[]>(`/tags/getTags/${docPath}`).toPromise()
        );
        this.studentGroupName = "";
        let groupSep = "";
        if (r.ok) {
            const tags = r.result;
            for (const tag of tags) {
                if (tag.type === TagType.CourseCode) {
                    this.currentCode = tag;
                }
                if (tag.type === TagType.Subject) {
                    this.currentSubject = tag;
                }
                if (tag.name.startsWith(groupTagPrefix)) {
                    this.studentGroupName +=
                        groupSep + tag.name.slice(groupTagPrefix.length);
                    groupSep = ";";
                }
            }
        }
    }

    /**
     * Removes current code and subject tags so they can be replaced.
     */
    private async removeCurrentSpecialTags() {
        const docPath = this.data.path;
        const r = await to2(
            this.http
                .post(`/tags/setCourseTags/${docPath}`, {
                    groups: [],
                    tags: [],
                })
                .toPromise()
        );
        if (!r.ok) {
            this.errorMessage = r.result.error.error;
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
            this.expires = this.currentSubject.expires?.toDate();
        } else {
            this.courseSubject = "";
            this.courseCode = "";
            this.expires = undefined;
        }
    }

    /**
     * Removes course code and subject from the database.
     */
    async unregisterCourse() {
        await this.removeCurrentSpecialTags();
        this.updateFields();
        this.errorMessage = undefined;
        this.successMessage = "Course code and subject successfully removed";
    }

    /**
     * Creates course special tags and adds them to database.
     */
    async registerCourse() {
        if (this.f.invalid) {
            return;
        }
        const docPath = this.data.path;
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
            expires: this.expires,
            name: codeName,
            type: TagType.CourseCode,
        };
        const subjectTag = {
            expires: this.expires,
            name: this.courseSubject.trim(),
            type: TagType.Subject,
        };

        const r = await to2(
            this.http
                .post(`/tags/setCourseTags/${docPath}`, {
                    groups:
                        this.studentGroupName.length > 0
                            ? this.studentGroupName.split(";")
                            : [],
                    tags: [codeTag, subjectTag],
                })
                .toPromise()
        );
        if (!r.ok) {
            this.errorMessage = r.result.error.error;
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
        const r = await to2(
            this.http.get<ICourseSettings>(`/courses/settings`).toPromise()
        );
        if (r.ok) {
            // Add a placeholder subject for quicker testing in case course settings does not exist.
            this.subjects = r.result.course_subjects ?? ["test"];
            this.subjects.sort();
        } else {
            this.errorMessage = r.result.error.error;
            this.successMessage = undefined;
        }
    }
}

@NgModule({
    declarations: [CourseDialogComponent],
    imports: [
        BrowserModule,
        DialogModule,
        FormsModule,
        TooltipModule.forRoot(),
        DatetimePickerModule,
    ],
})
export class CourseDialogModule {}
