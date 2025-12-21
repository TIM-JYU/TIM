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

export interface InputDialogResult {
    ok: boolean;
    value: string;
    selected: boolean[];
    selectedIndex: number;
}

export type InputDialogParams<T> = {
    title: string;
    text: string;
    okText?: string;
    cancelText?: string;
    asyncContent?: boolean;
    defaultValue?: string;
    options?: string[];
    selected?: boolean[];
    selectedIndex?: number;
    validator?: (s: string) => Promise<Result<T, string>>;
} & (
    | {isInput?: InputDialogKind.NoValidator; okValue: T}
    | {
          isInput?: InputDialogKind.ValidatorOnly;
          validator: () => Promise<Result<T, string>>;
      }
    | {
          isInput?: InputDialogKind.InputAndValidator;
      }
) &
    (
        | {
              inputType?: "select";
          }
        | {
              inputType?: "textarea";
          }
        | {
              inputType?: "text";
          }
        | {
              inputType?: "radio";
          }
        | {
              inputType?: "checkbox";
          }
    );

@Component({
    selector: "tim-input-dialog",
    template: `
        <div class="modal-bg">
        </div>
        <tim-dialog-frame class="input-dialog" [minimizable]="false" [mightBeAsync]=asyncContent [showCloseIcon]="!loading">
            <ng-container header>
                {{getTitle()}}
            </ng-container>
            <ng-container body>
                <p tabindex="-1" #textEl [innerHtml]="text()" class="content"></p>
                <textarea
                       class="form-control"
                       focusMe
                       [disabled]="loading"
                       type="text"
                       *ngIf="isInput && (data.inputType === 'textarea')"
                       [(ngModel)]="value" 
                          (ngModelChange)="clearError()"></textarea>
                <input (keydown.enter)="ok()"
                       class="form-control"
                       focusMe
                       [disabled]="loading"
                       type="text"
                       *ngIf="isInput && (data.inputType === 'text' || data.inputType === undefined)"
                       [(ngModel)]="value"
                          (ngModelChange)="clearError()" />
                <div [ngSwitch]="data.inputType">
                  <!-- select -->
                  <select *ngSwitchCase="'select'" 
                    (keydown.enter)="ok()"
                    class="option"
                    focusMe
                    [disabled]="loading"
                    [(ngModel)]="value"
                    (ngModelChange)="clearError()">
                     <option *ngFor="let option of selectOptions" [value]="option">{{ option }}</option>
                  </select>
            
                  <!-- radio -->
                  <div *ngSwitchCase="'radio'">
                    <label class="option" *ngFor="let option of selectOptions">
                      <input
                        type="radio"
                        name="inputRadio"
                        focusMe
                        [value]="option"
                        [(ngModel)]="value"
                      />
                      {{ option }}<br>
                    </label>
                  </div>
            
                  <!-- checkbox (multi-select) -->
                  <div *ngSwitchCase="'checkbox'">
                    <label class="option"  *ngFor="let option of selectOptions">
                      <input
                        type="checkbox"
                        focusMe
                        [checked]="selectedCheckboxes.includes(option)"
                        (change)="onCheckboxChange(option, $event)"
                      />
                      {{ option }}<br>
                    </label>
                  </div>
                </div>
                <!---
                <select (keydown.enter)="ok()"
                        class="form-control"
                        focusMe
                        [disabled]="loading"
                        *ngIf="isInput && data.inputType === 'select'"
                        [(ngModel)]="value"
                        (ngModelChange)="clearError()">
                    <option *ngFor="let option of selectOptions" [value]="option">{{option}}</option>
                </select> -->
                <tim-alert *ngIf="error" severity="danger">
                    {{ error }}
                </tim-alert>
            </ng-container>
            <ng-container footer>
                <tim-loading *ngIf="loading"></tim-loading>
                <button [disabled]="!value || loading"
                        class="timButton" type="button" (click)="ok()">{{ okText() }}
                </button>
                <button [disabled]="loading" class="btn btn-default" type="button" (click)="dismiss()">{{ cancelText() }}</button>
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
    loading = false;
    inputValue: string = "";
    selectedCheckboxes: string[] = [];
    options: string[] = [];

    @ViewChild("textEl") textEl!: ElementRef<HTMLElement>;

    @HostListener("keydown.enter", ["$event"])
    enterPressed(e: KeyboardEvent) {
        // Textarea is multiline, we cannot just skip the enter key
        if (this.data.inputType === "textarea") {
            return;
        }
        void this.ok();
        e.stopPropagation();
    }

    getTitle() {
        return this.data.title;
    }

    get selectOptions() {
        return this.options;
    }

    ngOnInit() {
        if (Array.isArray(this.data.options)) {
            this.options = this.data.options;
        }
        this.isInput = this.data.isInput == InputDialogKind.InputAndValidator;
        if (this.data.inputType !== undefined) {
            this.isInput = true;
        }
        if (this.isInput) {
            this.value = this.data.defaultValue ?? "";
        } else {
            this.value = "-";
        }
        if (this.data.asyncContent == false) {
            this.asyncContent = false;
        }
        // Narrow the union: ensure we have options and selected arrays before using them
        const d = this.data;
        if (Array.isArray(d.selected)) {
            let firstOption: string | undefined;

            // Look values from selected
            for (
                let i = 0;
                i < d.selected.length && i < this.options.length;
                i++
            ) {
                if (d.selected[i]) {
                    const option = this.options[i];
                    firstOption ??= option;
                    this.selectedCheckboxes.push(option);
                }
            }

            this.value = firstOption ?? this.value;
        }
        if (this.data.selectedIndex !== undefined) {
            const idx = this.data.selectedIndex;
            if (idx >= 0 && idx < this.options.length) {
                this.value = this.options[idx];
                this.selectedCheckboxes = [this.value];
            }
        }
    }

    ngAfterViewInit() {
        super.ngAfterViewInit();
        // Grab focus when there is no input, this allows keybindings to be processed by this modal
        if (!this.isInput) {
            this.textEl.nativeElement.focus();
        }
    }

    onCheckboxChange(option: string, event: Event) {
        const isChecked = (event.target as HTMLInputElement).checked;
        if (isChecked) {
            this.selectedCheckboxes.push(option);
        } else {
            this.selectedCheckboxes = this.selectedCheckboxes.filter(
                (item) => item !== option
            );
        }
    }

    async ok() {
        if (!this.value || this.loading) {
            return;
        }
        if (this.data.isInput === InputDialogKind.NoValidator) {
            this.close(this.data.okValue);
        }

        if (this.data.validator == undefined) {
            // This is mostly for select/radio/checkbox inputs
            // where no validation is needed
            let selectedIndex = this.options.indexOf(this.value);
            const selected: boolean[] = new Array(this.options.length).fill(
                false
            );
            if (this.data.inputType === "checkbox") {
                selectedIndex = -1;
                for (let i = this.options.length - 1; i >= 0; i--) {
                    const option = this.options[i];
                    if (this.selectedCheckboxes.includes(option)) {
                        selected[i] = true;
                        selectedIndex = i;
                        this.value = option;
                    }
                }
            } else if (selectedIndex >= 0) {
                selected[selectedIndex] = true;
            }
            this.close({
                ok: true,
                result: this.value,
                selectedIndex: selectedIndex,
                selected: selected,
            } as unknown as T);
            return;
        }

        this.loading = true;
        const result = await this.data.validator(this.value);
        this.loading = false;
        if (!result.ok) {
            this.error = result.result;
            return;
        }
        this.close(result.result);
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
    exports: [InputDialogComponent],
})
export class InputDialogModule {}
