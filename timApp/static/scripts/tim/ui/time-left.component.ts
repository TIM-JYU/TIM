import type {OnChanges, OnInit, SimpleChanges} from "@angular/core";
import {Component, Input} from "@angular/core";
import {documentglobals} from "tim/util/globals";
import type {ITimeLeftSettings} from "tim/document/IDocSettings";
import type {Moment} from "moment";
import moment from "moment";
import {showMessageDialog} from "tim/ui/showMessageDialog";
import {vctrlInstance} from "tim/document/viewctrlinstance";

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
        <span class="label label-default" [class.low-time]="isLowTime" [class.glow]="isGlowing">
            <ng-container i18n>Time left:</ng-container> <tim-countdown [displayUnits]="['d', 'h', 'm', 's']" [endTime]="endTimeMoment"
                                      [lowTimeThreshold]="settings.low_time_threshold"
                                      [syncInterval]="syncInterval"
                                      [syncIntervalDeviation]="syncIntervalDeviation"
                                      (onFinish)="onTimeUp()"
                                      (onLowTime)="onLowTime()"></tim-countdown>
        </span>
        <div class="low-time-warn alert alert-danger" *ngIf="isLowTime && showLowTimeMessage">
            <button type="button" class="close" aria-label="Close" (click)="showLowTimeMessage = false"><span
                    aria-hidden="true">&times;</span></button>
            <ng-container i18n>The time is about to run out, remember to save your answers.</ng-container>
        </div>
    `,
    styleUrls: ["./time-left.component.scss"],
})
export class TimeLeftComponent implements OnChanges, OnInit {
    @Input() endTime?: string;
    showLowTimeMessage = true;
    isLowTime = false;
    isGlowing = false;
    isTimeUp = false;
    settings: ITimeLeftSettings = {
        ...TIME_LEFT_DEFAULTS,
        ...documentglobals()?.docSettings?.timeLeft,
    };
    syncInterval = this.settings.sync_interval;
    syncIntervalDeviation = this.settings.sync_interval_deviation;
    endTimeMoment?: Moment;

    ngOnInit() {
        vctrlInstance?.listen("docViewInfoUpdate", (info) => {
            if (!info.right) {
                return;
            }
            if (!info.right.accessible_to) {
                return;
            }

            this.endTime = info.right.accessible_to.toISOString();
            this.endTimeMoment = moment(this.endTime);
        });
    }

    ngOnChanges(c: SimpleChanges) {
        if (c.endTime) {
            this.endTimeMoment = c.endTime.currentValue
                ? moment(c.endTime.currentValue)
                : undefined;
        }
    }

    onLowTime() {
        this.isLowTime = true;
        this.syncInterval = this.settings.low_time_sync_interval;
        this.syncIntervalDeviation = this.settings.low_time_sync_deviation;
        if (
            this.settings.low_time_glow_duration >=
            this.settings.low_time_glow_period
        ) {
            this.isGlowing = true;
        } else {
            this.glow();
        }
    }

    async glow() {
        while (true) {
            this.isGlowing = true;
            if (await this.wait(this.settings.low_time_glow_duration)) {
                return;
            }
            this.isGlowing = false;
            if (await this.wait(this.settings.low_time_glow_period)) {
                return;
            }
        }
    }

    private wait(seconds: number) {
        return new Promise<boolean>((resolve) =>
            setTimeout(() => resolve(this.isTimeUp), seconds * 1000)
        );
    }

    onTimeUp() {
        this.isLowTime = false;
        this.isGlowing = false;
        this.isTimeUp = true;
        showMessageDialog(
            $localize`Time is up. You can still save answers for a while, but any new saves will be marked as late.`
        );
    }
}
