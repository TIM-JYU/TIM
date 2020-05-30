import {Component, Input} from "@angular/core";
import {showMessageDialog} from "tim/ui/dialog";
import {documentglobals} from "tim/util/globals";
import {ITimeLeftSettings} from "tim/document/IDocSettings";

const TIME_LEFT_DEFAULTS: ITimeLeftSettings = {
    lowTimeThreshold: 60,
};

@Component({
    selector: "tim-time-left",
    template: `
        <span class="label label-default" [class.low-time]="isLowTime" i18n>
      Time left: <tim-countdown [displayUnits]="['d']" [endTime]="endTime"
                                [lowTimeThreshold]="settings.lowTimeThreshold" (onFinish)="onTimeUp()"
                                (onLowTime)="onLowTime()"></tim-countdown>
    </span>
        <div class="low-time-warn alert alert-danger" *ngIf="isLowTime && showLowTimeMessage" i18n>
            <button type="button" class="close" aria-label="Close" (click)="showLowTimeMessage = false"><span aria-hidden="true">&times;</span></button>
            The time is about to run out, remember to save your answers.
        </div>
        <ng-template i18n="@@timeUpMessage">Time is up. You can still save answers for a while, but any new saves will be
            marked as late.
        </ng-template>
    `,
    styleUrls: ["./time-left.component.scss"],
})
export class TimeLeftComponent {
    @Input() endTime?: string;
    showLowTimeMessage = true;
    isLowTime = false;
    settings: ITimeLeftSettings = {...TIME_LEFT_DEFAULTS, ...documentglobals()?.docSettings?.timeLeft};

    onLowTime() {
        this.isLowTime = true;
    }

    onTimeUp() {
        this.isLowTime = false;
        showMessageDialog($localize`:@@timeUpMessage:Time is up. You can still save answers for a while, but any new saves will be marked as late.`);
    }
}
