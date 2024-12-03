import * as t from "io-ts";
import type {ApplicationRef, DoBootstrap} from "@angular/core";
import {Component, NgModule} from "@angular/core";
import {CommonModule} from "@angular/common";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {PurifyModule} from "tim/util/purify.module";
import {type AngularError, type Result, toPromise} from "tim/util/utils";
import type {TaskId} from "tim/plugin/taskid";
import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "tim/plugin/attributes";
import {registerPlugin} from "tim/plugin/pluginRegistry";
import {AngularPluginBase} from "tim/plugin/angular-plugin-base.directive";

const TSteps = t.type({
    name: t.string,
    description: nullable(t.string),
});

type Steps = t.TypeOf<typeof TSteps>;

const PluginMarkupFields = t.intersection([
    GenericPluginMarkup,
    t.type({
        steps: withDefault(t.array(TSteps), []),
    }),
]);

const PluginFields = t.intersection([
    getTopLevelFields(PluginMarkupFields),
    t.type({
        state: nullable(
            t.type({
                current_step: t.number,
            })
        ),
    }),
]);

interface StepsState {
    id: number;
    documentId: number;
    user_working_group: number;
    user_group: number;
    name: string;
    current_phase: number;
}

@Component({
    selector: "tim-steps-plugin",
    template: `
        <div class="steps-wrapper">
            <div class="tim-steps-container">
                <div class="grid-container">
                    <div class="profile-heading left-column">
                        <h2>Step-by-step</h2>
                    </div>
                    <div class="right-column flex justify-corner-button">
                        <!-- <button class="timButton btn-danger corner-button">Remove</button>
                        <button (click)="onSubmit()" class="timButton btn-success corner-button">Save</button> -->
                    </div>
                </div>
                

                <div class="container">
                    <div class="step-list">
                        <div [style.opacity]="active ? '100%' : '50%'"
                             [style.background-color]="" class="step " *ngFor="let step of steps; index as i">
                        <span class="step-num"
                              [ngStyle]="currentStep >= i ? {'background-color': '#0F4096', 'color': '#ffefef'} : {}"
                              (click)="switchComplete($event, i)">{{ i + 1 }}</span>
                            <h3>{{ step.name }}</h3>
                            <div class="step-content"
                                 [style.border-left-style]="currentStep >= i ? 'solid' : 'dashed'">
                                <p [innerHTML]="step.description | purify"></p>
                            </div>
                        </div>
                        <div class="step"
                             [style.opacity]="active ? '100%' : '50%'">
                        <span class="step-num last-step-num glyphicon glyphicon-ok"
                        [ngStyle]="currentStep >= steps.length ? {'background-color': '#00AA00', 'color': '#ffefef'} : {}"
                              (click)="switchComplete($event, steps.length)">
                        </span>
                            <h3>{{ lastStep?.name }}</h3>
                            <p *ngIf="lastStep?.description" [innerHTML]="lastStep?.description | purify"></p>
                        </div>
                    </div>
                </div>
                <ng-content></ng-content>
            </div>
        </div>
    `,
    styleUrls: ["./steps.component.scss"],
})
export class StepsPluginComponent extends AngularPluginBase<
    t.TypeOf<typeof PluginMarkupFields>,
    t.TypeOf<typeof PluginFields>,
    typeof PluginFields
> {
    editable: boolean = false;
    active: boolean = true;
    isCompleted: boolean = false;
    steps: Steps[] = [];
    lastStep: Steps | undefined;
    currentStep: number = 0;

    ngOnInit() {
        super.ngOnInit();
        this.steps = this.markup.steps;
        this.lastStep = this.steps.pop();
        this.isCompleted = !!this.attrsall.state?.current_step;
        this.getStepsState();
    }

    async switchComplete(event: Event, index: number) {
        // Switch step in the component view and assign it to the request data.
        this.currentStep = index;
        const data: TaskId | undefined = Object.assign({}, this.getTaskId(), {
            currentStep: this.currentStep,
        });

        // Post the step phase change to database
        const endpoint = "/steps/switch";
        const response = toPromise(
            this.http.post<{ok: boolean}>(endpoint, data)
        );

        // Get result from response of the endpoint.
        const result: Result<{ok: boolean}, AngularError> = await response;
        console.log(result);

        return result;
    }

    getAttributeType() {
        return PluginFields;
    }

    getDefaultMarkup() {
        return {};
    }

    getStepsState(): void {
        const plugin: TaskId | undefined = this.getTaskId();

        // TODO: inform user about the problem, missing plugin or missing doc id
        if (plugin?.docId == undefined) {
            return;
        }

        const endpoint: string = `/steps/${plugin.docId}/${plugin.name}`;
        this.http.get<StepsState>(endpoint).subscribe({
            next: (res: StepsState) => {
                console.log(res);
                this.currentStep = res.current_phase;
            },
        });
        return;
    }
}

@NgModule({
    declarations: [StepsPluginComponent],
    imports: [CommonModule, HttpClientModule, FormsModule, PurifyModule],
})
export class StepsPluginModule implements DoBootstrap {
    ngDoBootstrap(appRef: ApplicationRef): void {}
}

registerPlugin("tim-steps-plugin", StepsPluginModule, StepsPluginComponent);
