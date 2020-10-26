import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import {Result} from "../util/utils";

export type InputDialogParams<T> = {
    title: string;
    text: string;
    okText?: string;
    cancelText?: string;
} & (
    | {isInput: InputDialogKind.NoValidator; okValue: T}
    | {
          isInput: InputDialogKind.ValidatorOnly;
          validator: () => Promise<Result<T, string>>;
      }
    | {
          isInput: InputDialogKind.InputAndValidator;
          defaultValue: string;
          validator: (s: string) => Promise<Result<T, string>>;
      }
);

@Component({
    selector: "tim-input-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{getTitle()}}
            </ng-container>
            <ng-container body>
                <p [innerHtml]="text()"></p>
                <input (keydown.enter)="ok()"
                       class="form-control"
                       focusMe
                       type="text"
                       *ngIf="isInput"
                       [(ngModel)]="value"
                       (ngModelChange)="clearError()">
                <tim-alert *ngIf="error" severity="danger">
                    {{ error }}
                </tim-alert>
            </ng-container>
            <ng-container footer>
                <button [disabled]="!value"
                        class="timButton" type="button" (click)="ok()">{{ okText() }}
                </button>
                <button class="btn btn-default" type="button" (click)="dismiss()">{{ cancelText() }}</button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class InputDialogComponent<T> extends AngularDialogComponent<
    InputDialogParams<T>,
    T
> {
    protected dialogName = "Input";
    value = "";
    error?: string;
    isInput = false;

    getTitle() {
        return this.data.title;
    }

    ngOnInit() {
        this.isInput = this.data.isInput == InputDialogKind.InputAndValidator;
        if (this.data.isInput !== InputDialogKind.InputAndValidator) {
            this.value = "-";
        } else {
            this.value = this.data.defaultValue;
        }
    }

    async ok() {
        if (!this.value) {
            return;
        }
        if (this.data.isInput !== InputDialogKind.NoValidator) {
            const result = await this.data.validator(this.value);
            if (!result.ok) {
                this.error = result.result;
                return;
            }
            this.close(result.result);
        } else {
            this.close(this.data.okValue);
        }
    }

    clearError() {
        this.error = undefined;
    }

    okText(): string {
        return this.data.okText ?? "OK";
    }

    cancelText(): string {
        return this.data.cancelText ?? "Cancel";
    }

    text() {
        return this.data.text;
    }
}

@NgModule({
    declarations: [InputDialogComponent],
    imports: [BrowserModule, DialogModule, TimUtilityModule, FormsModule],
})
export class InputDialogModule {}
