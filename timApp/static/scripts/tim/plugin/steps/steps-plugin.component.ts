import {
    GenericPluginMarkup,
    getTopLevelFields,
    nullable,
    withDefault,
} from "../attributes";
import * as t from "io-ts";
import {
    ApplicationRef,
    Component,
    DoBootstrap,
    Input,
    NgModule,
    OnInit,
} from "@angular/core";
import {AngularPluginBase} from "../angular-plugin-base.directive";
import {CommonModule} from "@angular/common";
import {HttpClientModule} from "@angular/common/http";
import {FormsModule} from "@angular/forms";
import {registerPlugin} from "../pluginRegistry";
import {PurifyModule} from "tim/util/purify.module";
import {type AngularError, type Result, toPromise} from "tim/util/utils";
import {TaskId} from "tim/plugin/taskid";
import {Subscription} from "rxjs";

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
                        <button class="timButton btn-danger corner-button">Remove</button>
                        <button (click)="onSubmit()" class="timButton btn-success corner-button">Save</button>
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
                                <p [innerHTML]="step.description | purify">This is the intermediate element</p>
                            </div>
                        </div>
                        <div class="step step-upcoming ">
                        <span class="step-num last-step-num glyphicon glyphicon-ok">
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
    documentId: number = 0;
    modifyEnabled: boolean = false;
    editable: boolean = false;
    active: boolean = true;
    isCompleted: boolean = false;
    stepsEndpoint: string = "/steps/create/";
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
        this.currentStep = index;
        const data: TaskId | undefined = Object.assign({}, this.getTaskId(), {
            currentStep: this.currentStep,
        });

        const endpoint = "/steps/switch";
        const response = toPromise(
            this.http.post<{ok: boolean}>(endpoint, data)
        );

        // Get result from response of the endpoint.
        const result: Result<{ok: boolean}, AngularError> = await response;
        console.log(result);

        /*
        const r = await this.postAnswer<{
            web: {result: string; error?: string};
        }>({
            input: {
                current_step: this.isCompleted ? 1 : 0,
            },
        });
        */

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
        let endpoint: string = `/steps/${plugin?.docId}/${plugin?.name}`;
        this.http.get<StepsState>(endpoint).subscribe({
            next: (res: StepsState) => {
                console.log(res);
                this.currentStep = res.current_phase;
            },
        });
        return;
    }

    async onSubmit() {
        console.log(this.markup.steps);
        const data = {};
        const endpoint = "/steps/create";
        // const response = toPromise(
        //    this.http.post<{ok: boolean}>(endpoint, data)
        //);

        // Get result from response of the endpoint.
        //const result: Result<{ok: boolean}, AngularError> = await response;
        //console.log(result);
        //return result;
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
