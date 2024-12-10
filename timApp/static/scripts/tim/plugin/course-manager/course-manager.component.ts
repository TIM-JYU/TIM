import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Input, NgModule} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {type AngularError, type Result, toPromise} from "tim/util/utils";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";

@Component({
    selector: "tim-course-manager",
    styleUrls: ["./course-manager.component.scss"],
    imports: [CsUtilityModule, FormsModule, CommonModule],
    standalone: true,
    template: `
        <div class="tim-course-manager-container">
            <div class="container">
                <form (ngSubmit)="onSubmit()" #f="ngForm">
                    <input name="course" class="form-control" type="text"
                           (ngModelChange)="setWarning('course')"
                           [(ngModel)]="copyPath"
                           [placeholder]="'Copy from path, eg. courses/camp'"
                           [class.warnFrame]="checkWarning('course')" /> 
                    <input name="link" class="form-control" type="text"
                           (ngModelChange)="setWarning('link')"
                           [(ngModel)]="courseName"
                           [placeholder]="'Name for a copy'"
                           [class.warnFrame]="checkWarning('link')" /> 
                    <button type="submit" class="btn">Create <span class="glyphicon glyphicon-send"></span></button>
                </form>
            </div>
            <div class="alert alert-success" *ngIf="isCourseCreated"><span class="msg-icon glyphicon glyphicon-ok"></span> <a [href]="folderCreatedUrl">Link to the course</a></div>
            <div *ngIf="fail" class="alert alert-warning flex"><span class="msg-icon glyphicon glyphicon-info-sign"></span><p>{{ failMessage }}</p></div>
        </div>
    `,
})
export class CourseManagerComponent implements OnInit {
    @Input() documentId: number = 0;
    @Input() modifyEnabled: boolean = false;
    editable: boolean = false;
    initValue: string = "";
    courseName: string = "";
    copyPath: string = "";
    courseManagerEndpoint = "/courses/from-template";
    isCourseCreated: boolean = false;
    folderCreatedUrl: string = "";
    fail: boolean = false;
    failMessage: string = "";
    warnings: string[] = [];
    constructor(private http: HttpClient) {}

    ngOnInit() {}

    async onSubmit() {
        // Clear error element
        this.fail = false;

        // Prepare data for submit
        const data: {
            copy_to_dir_name: string;
            // copy_from_id: number | null;
            copy_from_path: string;
        } = {
            copy_to_dir_name: this.courseName,
            // copy_from_id: null,
            copy_from_path: this.copyPath,
        };

        // Call endpoint, which handles storing the data into document settings
        const response = toPromise(
            this.http.post<{ok: boolean}>(`${this.courseManagerEndpoint}`, data)
        );

        response.then((res) => {
            console.log(res);
        });

        // Get result from response of the endpoint.
        const result: Result<{ok: boolean}, AngularError> = await response;

        this.isCourseCreated = result.ok;

        if (result.ok) {
            this.folderCreatedUrl = `/view/oscar/camps/${data.copy_to_dir_name}`;
        } else {
            const msg: string = result.result.error.error;
            this.fail = true;
            this.failMessage = msg;
            // await showMessageDialog();
        }

        return result;
    }

    setWarning(name: string) {
        if (!this.warnings.includes(name)) {
            this.warnings.push(name);
        }
    }

    checkWarning(name: string) {
        return this.warnings.includes(name);
    }
}

@NgModule({
    imports: [CourseManagerComponent],
})
export class CourseManagerModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
