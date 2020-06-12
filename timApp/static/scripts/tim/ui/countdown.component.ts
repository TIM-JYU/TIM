import {Component, EventEmitter, Input, OnInit, Output} from "@angular/core";
import humanizeDuration from "humanize-duration";
import {formatString, secondsToHHMMSS, to2} from "tim/util/utils";
import {Users} from "tim/user/userService";
import moment from "moment";
import {HttpClient} from "@angular/common/http";

const DAY_LIMIT = 24 * 60 * 60;

@Component({
    selector: "tim-countdown",
    template: `
        {{timeLeftText}}
    `,
})
export class CountdownComponent implements OnInit {
    @Input() endTime?: string;
    @Input() seconds?: number;
    @Input() displayUnits: humanizeDuration.Unit[] = [];
    @Input() noAutoStart: boolean = false;
    @Input() template: string = "{0}";
    @Input() lowTimeThreshold: number = -1;
    @Input() syncInterval: number = -1;
    @Input() tickInterval: number = 1;
    @Input() syncIntervalDeviation: number = 0;
    @Output() onFinish: EventEmitter<void> = new EventEmitter<void>();
    @Output() onLowTime: EventEmitter<void> = new EventEmitter<void>();

    nextSyncInterval = -1;
    running = false;
    isLowTime = false;
    currentCountdown = 0;
    currentEndDate?: moment.Moment;
    locale = Users.getCurrentLanguage();
    currentInterval?: number;
    timeLeftText?: string;
    lastSync?: moment.Moment;

    constructor(private http: HttpClient) {
    }

    get timeLeft() {
        let prefix = "";
        // We need time as a whole number so we won't render fractional parts
        let time = Math.ceil(Math.max(this.currentCountdown, 0));
        if (time > DAY_LIMIT && this.displayUnits.length != 0) {
            prefix = humanizeDuration(time * 1000, {
                units: this.displayUnits,
                round: true,
                language: this.locale,
            }) + " + ";
            time %= DAY_LIMIT;
        }
        return `${prefix}${secondsToHHMMSS(time)}`;
    }

    private async getEndDate() {
        if (this.seconds) {
            return moment().add(this.seconds, "s");
        }
        if (this.endTime) {
            const endDate = await this.getSyncedEndDate();
            if (endDate) {
                return endDate;
            }
        }
        return moment();
    }

    private async getSyncedEndDate() {
        const start = window.performance.now();
        const serverTime = await to2(this.http.get<{ time: moment.Moment }>("/time").toPromise());
        if (!serverTime.ok) {
            return undefined;
        }
        const remaining = moment(this.endTime).diff(serverTime.result.time, "ms", true);
        const end = window.performance.now();

        // Attempt to alleviate potential error due to the RTT of the request by subtracting the run time of
        // the /time request + all processing
        // Note that all timing in JS is inherently imprecise on purpose (but should be enough for our needs):
        // https://developer.mozilla.org/en-US/docs/Web/API/Performance/now#Reduced_time_precision
        return moment().add(remaining - (end - start), "ms");
    }

    private async syncEndDate() {
        // We're on set countdown, nothing to sync against
        if (!this.endTime) {
            return;
        }
        const endDate = await this.getSyncedEndDate();
        // Update the end date only if it was synced successfully with the server
        if (endDate) {
            this.currentEndDate = endDate;
        }
        this.updateSyncInterval();
    }

    ngOnInit() {
        if (this.noAutoStart) {
            return;
        }
        this.start();
    }

    async start() {
        if (this.currentInterval) {
            return;
        }
        this.currentEndDate = await this.getEndDate();
        this.updateSyncInterval();
        this.startTimer();
    }

    private async startTimer() {
        if (this.running) {
            return;
        }
        this.running = true;
        await this.checkCountdown();
        const tick = async () => {
            if (!this.running) {
                return;
            }
            await this.checkCountdown();
            setTimeout(tick, this.tickInterval * 1000);
        };
        setTimeout(tick, this.tickInterval * 1000);
    }

    stop() {
        if (!this.running) {
            return;
        }
        this.running = false;
    }

    reset() {
        this.isLowTime = false;
    }

    private async checkCountdown() {
        const now = moment();
        this.currentCountdown = this.currentEndDate?.diff(now, "s", true) ?? 0;
        this.timeLeftText = formatString(this.template, this.timeLeft);
        if (this.nextSyncInterval > 0 && now.diff(this.lastSync, "s") >= this.nextSyncInterval) {
            await this.syncEndDate();
        }
        const timeEnded = this.currentCountdown <= 0;
        if (!this.isLowTime && this.currentCountdown < this.lowTimeThreshold) {
            this.onLowTime.emit();
            this.isLowTime = true;
            await this.syncEndDate();
        }
        if (timeEnded) {
            this.onFinish.emit();
            this.stop();
        }
    }

    private updateSyncInterval() {
        this.lastSync = moment();
        if (this.syncInterval <= 0) {
            return;
        }
        this.nextSyncInterval = this.syncInterval;
        if (this.syncIntervalDeviation > 0) {
            this.nextSyncInterval *= 1 + (2 * Math.random() - 1) * this.syncIntervalDeviation;
        }
    }
}
