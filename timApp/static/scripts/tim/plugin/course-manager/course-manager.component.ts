import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Input, NgModule} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {type AngularError, type Result, toPromise} from "tim/util/utils";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";

interface TemplateData {
    copy_to_path: string;
    copy_from_path: string;
}

@Component({
    selector: "tim-course-manager",
    styleUrls: ["./course-manager.component.scss"],
    imports: [CsUtilityModule, FormsModule, CommonModule],
    standalone: true,
    template: `
        <div class="tim-course-manager-container">
            <div class="container">
                <form (ngSubmit)="onSubmit()" #f="ngForm">
                    <input name="template" class="form-control" type="text"
                           (ngModelChange)="setWarning('template')"
                           [(ngModel)]="copyPath"
                           [placeholder]="'Copy from path, eg. courses/camp'"
                           [class.warnFrame]="checkWarning('template')"/>
                    <input name="name" class="form-control" type="text"
                           (ngModelChange)="setWarning('name')"
                           [(ngModel)]="coursePath"
                           [placeholder]="'Path for a copy'"
                           [class.warnFrame]="checkWarning('name')"/>
                    <button type="submit" class="btn">Create <span class="glyphicon glyphicon-send"></span></button>
                </form>
            </div>
            <div class="alert alert-success" *ngIf="isCourseCreated"><span
                    class="msg-icon glyphicon glyphicon-ok"></span> <a [href]="folderCreatedUrl">Link to the course</a>
            </div>
            <div *ngIf="fail" class="alert alert-warning flex"><span
                    class="msg-icon glyphicon glyphicon-info-sign"></span>
                <p>{{ failMessage }}</p></div>
        </div>
    `,
})
export class CourseManagerComponent implements OnInit {
    @Input() placeholder: string = "";
    coursePath: string = "";
    copyPath: string = "";
    isCourseCreated: boolean = false;
    folderCreatedUrl: string = "";
    fail: boolean = false;
    failMessage: string = "";
    warnings: string[] = [];
    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.coursePath = this.placeholder.concat("/my-new-course");
    }

    async onSubmit() {
        const endpoint = "/courses/from-template";

        // Clear error element
        this.fail = false;

        // Prepare data for submit
        const data: TemplateData = {
            copy_to_path: this.coursePath,
            copy_from_path: this.copyPath,
        };

        // Call endpoint, which handles storing the data into document settings
        const response = toPromise(
            this.http.post<{ok: boolean}>(endpoint, data)
        );

        response.then((res) => {
            console.log(res);
        });

        // Get result from response of the endpoint.
        const result: Result<{ok: boolean}, AngularError> = await response;

        this.isCourseCreated = result.ok;

        if (result.ok) {
            this.folderCreatedUrl = `/view/${data.copy_to_path}`;
        } else {
            const msg: string = result.result.error.error;
            this.fail = true;
            this.failMessage = msg;
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
