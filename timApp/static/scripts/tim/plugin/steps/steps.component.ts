import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Input, NgModule} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";
import {StandaloneTextfieldComponent} from "../../../../../modules/fields/js/standalone-textfield.component";
import {type AngularError, type Result, toPromise} from "tim/util/utils";
import {int} from "@zxing/library/es2015/customTypings";

interface StepsContent extends Object {}

interface Steps extends Object {
    id: number;
    name: string;
}

@Component({
    selector: "tim-steps",
    styleUrls: ["./steps.component.scss"],
    imports: [
        StandaloneTextfieldComponent,
        CsUtilityModule,
        FormsModule,
        CommonModule,
    ],
    standalone: true,
    template: `
        <div class="steps-wrapper">
            <div class="tim-steps-container">
                <h2>Step-by-step configuration</h2>
            <div class="steps-selector-container">
                <h3>My step components</h3>
                <form action="" class="grid-container">
                    <div class="left-column">
                                        <select class="form-control">
                    <ng-container *ngFor="let step of steps">
                        <option [value]="step.id">{{ step.name }}</option>
                    </ng-container>
                </select>
                    </div>
                    <div class="right-column">
                        <button (click)="onSubmit()" class="timButton btn-success">Add</button>
                    </div>
                </form>
            </div>
            </div>
        <div class="tim-steps-container">
            <div class="grid-container">
            <div class="profile-heading left-column">
                <h2>Step-by-step</h2>
            </div>    
                <div class="right-column flex justify-corner-button">
                    <button class="timButton btn-danger corner-button">Remove</button>
                </div>
            </div>
            
            <div class="container">
                <div class="step-list">
                    <div [style.opacity]="active ? '100%' : '50%'" 
                         [style.background-color]="" class="step ">
                        <span class="step-num"
                              [ngStyle]="isCompleted ? {'background-color': '#0F4096', 'color': '#ffefef'} : {}"
                              (click)="switchComplete($event)">1</span>
                        <h3>Step one</h3>
                        <div class="step-content"
                            [style.border-left-style]="isCompleted ? 'solid' : 'dashed'" >
                            <p>Content.</p>
                            <p>More content.</p>
                            <p>And more.</p>
                        </div>
                    </div>
                    <div class="step step-upcoming ">
                        <span class="step-num last-step-num glyphicon glyphicon-ok">
                        </span>
                        <h3>Last step</h3>
                    </div>
                </div>
            </div>
            <ng-content></ng-content>
        </div>
            </div>
                                <!--                              [style.background-color]="isCompleted ? '#0F4096' : '#0F409620'"-->
                        <!--                              [style.color]="isCompleted ? '#ffefef' : '#0F0C00'"-->
    `,
})
export class StepsComponent implements OnInit {
    @Input() documentId: number = 0;
    @Input() modifyEnabled: boolean = false;
    editable: boolean = false;
    active: boolean = true;
    isCompleted: boolean = false;
    stepsEndpoint: string = "/steps/create/";
    steps: Steps[] | [] = [];

    constructor(private http: HttpClient) {}

    ngOnInit() {
        this.getStepsContent(0);
        this.getSteps();
        console.log(this.documentId);
    }

    modifySteps() {
        console.log("Steps modify.");
    }

    getStepsContent(stepsId: number) {
        let endpoint = "";

        const data = this.http.get<StepsContent>(endpoint).subscribe({
            next: (res: StepsContent) => {
                console.log(res);
            },
        });
        return data;
    }

    getSteps() {
        let endpoint = "/steps/";
        const data = this.http.get<StepsContent>(endpoint).subscribe({
            next: (res: StepsContent) => {
                console.log(res);
                this.steps = [
                    {
                        id: 0,
                        name: "dummy0",
                    },
                    {
                        id: 1,
                        name: "dummy1",
                    },
                    {
                        id: 2,
                        name: "dummy2",
                    },
                ];
            },
        });
        return data;
    }

    switchComplete(event: Event) {
        this.isCompleted = !this.isCompleted;
    }

    async onSubmit() {
        console.log(this.documentId);
        const data = {md: "# Dummy steps\nContent.", doc_id: this.documentId};
        const endpoint = "/edit_page/newParagraph/";
        const response = toPromise(
            this.http.post<{ok: boolean}>(endpoint, data)
        );

        // Get result from response of the endpoint.
        const result: Result<{ok: boolean}, AngularError> = await response;
        console.log(result);
        return result;
    }
}

@NgModule({
    imports: [StepsComponent],
})
export class StepsModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
