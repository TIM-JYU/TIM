import type {ApplicationRef, DoBootstrap, OnInit} from "@angular/core";
import {Input, NgModule} from "@angular/core";
import {Component} from "@angular/core";
import {HttpClient} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {CommonModule} from "@angular/common";
import {CsUtilityModule} from "../../../../../modules/cs/js/util/module";
import {StandaloneTextfieldComponent} from "../../../../../modules/fields/js/standalone-textfield.component";

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
        <div class="tim-steps-container">
            <div class="profile-heading">
                <h2>Step-by-step</h2>
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

    constructor(private http: HttpClient) {}

    ngOnInit() {}

    async onSubmit() {}

    modifySteps() {
        console.log("Steps modify.");
    }

    switchComplete(event: Event) {
        this.isCompleted = !this.isCompleted;
    }
}

@NgModule({
    imports: [StepsComponent],
})
export class StepsModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef) {}
}
