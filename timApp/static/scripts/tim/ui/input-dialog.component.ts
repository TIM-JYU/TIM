import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {
    Component,
    ElementRef,
    HostListener,
    NgModule,
    ViewChild,
} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {TimUtilityModule} from "tim/ui/tim-utility.module";
import {FormsModule} from "@angular/forms";
import {InputDialogKind} from "tim/ui/input-dialog.kind";
import type {Result} from "tim/util/utils";
import {CommonModule} from "@angular/common";

export type InputDialogParams<T> = {
    title: string;
    text: string;
    okText?: string;
    cancelText?: string;
    asyncContent?: boolean;
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
) &
    (
        | {
              inputType?: "select";
              options: string[];
          }
        | {
              inputType?: "textarea";
          }
    );

@Component({
    selector: "tim-input-dialog",
    template: `
        <div class="modal-bg">
        </div>
        <tim-dialog-frame [minimizable]="false" [mightBeAsync]=asyncContent>
            <ng-container header>
                {{getTitle()}}
            </ng-container>
            <ng-container body>
                <p tabindex="-1" #textEl [innerHtml]="text()"></p>
                <input (keydown.enter)="ok()"
                       class="form-control"
                       focusMe
                       type="text"
                       *ngIf="isInput && (data.inputType === 'textarea' || data.inputType === undefined)"
                       [(ngModel)]="value"
                       (ngModelChange)="clearError()">
                <select (keydown.enter)="ok()"
                        class="form-control"
                        focusMe
                        *ngIf="isInput && data.inputType === 'select'"
                        [(ngModel)]="value"
                        (ngModelChange)="clearError()">
                    <option *ngFor="let option of selectOptions" [value]="option">{{option}}</option>
                </select>
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
    styleUrls: ["./input-dialog.component.scss"],
})
export class InputDialogComponent<T> extends AngularDialogComponent<
    InputDialogParams<T>,
    T
> {
    protected dialogName = "Input";
    value = "";
    error?: string;
    isInput = false;
    asyncContent = true;
    @ViewChild("textEl") textEl!: ElementRef<HTMLElement>;

    @HostListener("keydown.enter", ["$event"])
    enterPressed(e: KeyboardEvent) {
        void this.ok();
        e.stopPropagation();
    }

    getTitle() {
        return this.data.title;
    }

    get selectOptions() {
        if (this.data.inputType !== "select") {
            return [];
        }
        return this.data.options;
    }

    ngOnInit() {
        this.isInput = this.data.isInput == InputDialogKind.InputAndValidator;
        if (this.data.isInput !== InputDialogKind.InputAndValidator) {
            this.value = "-";
        } else {
            this.value = this.data.defaultValue;
        }
        if (this.data.asyncContent == false) {
            this.asyncContent = false;
        }
    }

    ngAfterViewInit() {
        super.ngAfterViewInit();
        // Grab focus when there is no input, this allows keybindings to be processed by this modal
        if (!this.isInput) {
            this.textEl.nativeElement.focus();
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
        return this.data.cancelText ?? $localize`Cancel`;
    }

    text() {
        return this.data.text;
    }
}

@NgModule({
    declarations: [InputDialogComponent],
    imports: [CommonModule, DialogModule, TimUtilityModule, FormsModule],
})
export class InputDialogModule {}
