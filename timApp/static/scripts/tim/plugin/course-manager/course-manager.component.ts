import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Input, NgModule} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {type AngularError, type Result, toPromise} from "tim/util/utils";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";
import {StandaloneTextfieldComponent} from "../../../../../modules/fields/js/standalone-textfield.component";

@Component({
    selector: "tim-course-manager",
    styleUrls: ["./course-manager.component.scss"],
    imports: [
        StandaloneTextfieldComponent,
        CsUtilityModule,
        FormsModule,
        CommonModule,
    ],
    standalone: true,
    template: `
        <div class="tim-course-manager-container">
            <div class="heading">
                <h2>Course manager</h2>
            </div>
            <div class="container">
                <p>
                    Create a course from a template.
                </p>
                <form (ngSubmit)="onSubmit()" #f="ngForm">
                    <tim-standalone-textfield name="link" inputType="TEXT" 
                                          [initialValue]="initValue"
                                              [placeholder]="'Copy from path'"
                                          (valueChange)="updatePath($event)"
                    ></tim-standalone-textfield>
                <tim-standalone-textfield name="link" inputType="TEXT" 
                                          [initialValue]="initValue"
                                          [placeholder]="'Camp name'"
                                          (valueChange)="updateName($event)"
                    ></tim-standalone-textfield>
                    <button type="submit" class="btn">Create <span class="glyphicon glyphicon-send"></span></button>
                </form>
                <div *ngIf="isCourseCreated"><a [href]="folderCreatedUrl">Link to the course</a></div>
            </div>
        </div>
    `,
})
export class CourseManagerComponent implements OnInit {
    @Input() documentId: number = 0;
    @Input() modifyEnabled: boolean = false;
    editable: boolean = false;
    initValue: string = "";
    courseName: string = "";
    copyPath: string = "oscar/sample-camp";
    courseManagerEndpoint = "/courses/from-template";
    isCourseCreated: boolean = false;
    folderCreatedUrl: string = "";

    constructor(private http: HttpClient) {}

    ngOnInit() {}

    async onSubmit() {
        // Prepare data for submit
        const data: {
            copy_to_dir_name: string;
            copy_from_id: number | null;
            copy_from_path: string;
        } = {
            copy_to_dir_name: this.courseName,
            copy_from_id: null,
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
        }

        return result;
    }

    updateName($event: string) {
        this.courseName = $event;
    }

    updatePath($event: string) {
        this.copyPath = $event;
    }
}

@NgModule({
    imports: [CourseManagerComponent],
})
export class CourseManagerModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
