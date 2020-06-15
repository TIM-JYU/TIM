import {Component, Input} from "@angular/core";
import {showMessageDialog} from "tim/ui/dialog";
import {documentglobals} from "tim/util/globals";
import {ITimeLeftSettings} from "tim/document/IDocSettings";

const TIME_LEFT_DEFAULTS: ITimeLeftSettings = {
    sync_interval: 10 * 60,
    sync_interval_deviation: 0.2,
    low_time_threshold: 60,
    low_time_glow_period: 45,
    low_time_glow_duration: 15,
    low_time_sync_interval: 30,
    low_time_sync_deviation: 0.15,
};

@Component({
    selector: "tim-time-left",
    template: `
        <span class="label label-default" [class.low-time]="isLowTime" [class.glow]="isGlowing" i18n>
            Time left: <tim-countdown [displayUnits]="['d']" [endTime]="endTime"
                                      [lowTimeThreshold]="settings.low_time_threshold"
                                      [syncInterval]="syncInterval"
                                      [syncIntervalDeviation]="syncIntervalDeviation"
                                      (onFinish)="onTimeUp()"
                                      (onLowTime)="onLowTime()"></tim-countdown>
        </span>
        <div class="low-time-warn alert alert-danger" *ngIf="isLowTime && showLowTimeMessage" i18n>
            <button type="button" class="close" aria-label="Close" (click)="showLowTimeMessage = false"><span
                    aria-hidden="true">&times;</span></button>
            The time is about to run out, remember to save your answers.
        </div>
        <ng-template i18n="@@timeUpMessage">
            Time is up. You can still save answers for a while, but any new saves will be marked as late.
        </ng-template>
    `,
    styleUrls: ["./time-left.component.scss"],
})
export class TimeLeftComponent {
    @Input() endTime?: string;
    showLowTimeMessage = true;
    isLowTime = false;
    isGlowing = false;
    isTimeUp = false;
    settings: ITimeLeftSettings = {...TIME_LEFT_DEFAULTS, ...documentglobals()?.docSettings?.timeLeft};
    syncInterval = this.settings.sync_interval;
    syncIntervalDeviation = this.settings.sync_interval_deviation;

    onLowTime() {
        this.isLowTime = true;
        this.syncInterval = this.settings.low_time_sync_interval;
        this.syncIntervalDeviation = this.settings.low_time_sync_deviation;
        if (this.settings.low_time_glow_duration >= this.settings.low_time_glow_period) {
            this.isGlowing = true;
        } else {
            this.glow();
        }
    }

    async glow() {
        while (true) {
            this.isGlowing = true;
            if (await this.wait(this.settings.low_time_glow_duration)) { return; }
            this.isGlowing = false;
            if (await this.wait(this.settings.low_time_glow_period)) { return; }
        }
    }

    private wait(seconds: number) {
        return new Promise<boolean>((resolve) => setTimeout(() => resolve(this.isTimeUp), seconds * 1000));
    }

    onTimeUp() {
        this.isLowTime = false;
        this.isGlowing = false;
        this.isTimeUp = true;
        showMessageDialog($localize`:@@timeUpMessage:Time is up. You can still save answers for a while, but any new saves will be marked as late.`);
    }
}
