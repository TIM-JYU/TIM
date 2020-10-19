import {AngularDialogComponent} from "tim/ui/angulardialog/angular-dialog-component.directive";
import {Component, NgModule} from "@angular/core";
import {DialogModule} from "tim/ui/angulardialog/dialog.module";
import {BrowserModule} from "@angular/platform-browser";
import {FormsModule} from "@angular/forms";
import {hasLectureEnded, ILecture} from "./lecturetypes";

export interface ILectureEndingDialogResult {
    extendTime: number;
    result: "extend" | "dontextend" | "end";
}

@Component({
    selector: "tim-lecture-ending-dialog",
    template: `
        <tim-dialog-frame>
            <ng-container header>
                {{ getTitle() }}
            </ng-container>
            <ng-container body>
                <form>
                    <label> Extend by
                        <select [(ngModel)]="selectedTime">
                            <option *ngFor="let choice of extendTimes" [value]="choice">{{choice}}</option>
                        </select>
                        minutes
                    </label>
                </form>
            </ng-container>
            <ng-container footer>
                <button class="timButton" autofocus (click)="extend()">Extend</button>
                <button class="timButton" *ngIf="!hasLectureEnded()" (click)="end()">End</button>
                <button
                        class="timButton"
                        *ngIf="hasLectureEnded()"
                        (click)="noExtend()">Don't extend
                </button>
            </ng-container>
        </tim-dialog-frame>
    `,
})
export class LectureEndingDialogComponent extends AngularDialogComponent<
    ILecture,
    ILectureEndingDialogResult
> {
    protected dialogName = "LectureEnding";
    extendTimes = [5, 10, 15, 30, 45, 60];
    selectedTime = 15;

    public getTitle() {
        return "Lecture ending";
    }

    public noExtend() {
        this.close({result: "dontextend", extendTime: this.selectedTime});
    }

    public extend() {
        this.close({result: "extend", extendTime: this.selectedTime});
    }

    public end() {
        this.close({result: "end", extendTime: this.selectedTime});
    }

    public hasLectureEnded() {
        return hasLectureEnded(this.data);
    }
}

@NgModule({
    declarations: [LectureEndingDialogComponent],
    imports: [BrowserModule, DialogModule, FormsModule],
})
export class LectureEndingDialogModule {}
