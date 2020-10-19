import {
    Component,
    EventEmitter,
    Input,
    OnChanges,
    OnInit,
    Output,
} from "@angular/core";
import humanizeDuration from "humanize-duration";
import {
    formatString,
    ReadonlyMoment,
    secondsToShortTime,
    to2,
} from "tim/util/utils";
import {Users} from "tim/user/userService";
import moment from "moment";
import {HttpClient} from "@angular/common/http";
import {Changes} from "tim/util/angularchanges";

// Most browsers don't report precise time to mitigate potential time-based attacks/tracking:
// https://developer.mozilla.org/en-US/docs/Web/API/Performance/now#Reduced_time_precision
// Rounding to 1ms seems common, in which case the timeout can happen without 1ms window
const TIMEOUT_EPS = 0.001;
const DISPLAY_TENTHS_LIMIT = 10;
const TICK_SECOND = 1000;
const TICK_TENTH_SECOND = 100;

@Component({
    selector: "tim-countdown",
    template: `{{timeLeftText}}`,
})
export class CountdownComponent implements OnInit, OnChanges {
    @Input() endTime?: ReadonlyMoment;
    @Input() seconds?: number;
    @Input() displayUnits: humanizeDuration.Unit[] = ["h", "m", "s"];
    @Input() autoStart = true;
    @Input() template: string = "{0}";
    @Input() lowTimeThreshold?: number;
    @Input() syncInterval: number = -1;
    @Input() syncIntervalDeviation: number = 0;
    @Output() onFinish: EventEmitter<void> = new EventEmitter<void>();
    @Output() onLowTime: EventEmitter<void> = new EventEmitter<void>();
    @Output() onTick = new EventEmitter<number>();

    nextSyncInterval = -1;
    running = false;
    isLowTime = false;
    currentCountdown = 0;
    currentEndDate?: moment.Moment;
    locale = Users.getCurrentLanguage();
    currentInterval?: number;
    timeLeftText?: string;
    lastSync?: moment.Moment;

    constructor(private http: HttpClient) {}

    get timeLeft() {
        const clampedCountdown = Math.max(this.currentCountdown, 0);
        // We need time as a whole number so we won't render fractional parts
        const timeS = Math.floor(clampedCountdown);
        const msPostfix =
            timeS < DISPLAY_TENTHS_LIMIT
                ? `,${Math.trunc((clampedCountdown % 1) * 10)}`
                : "";
        return `${secondsToShortTime(
            timeS,
            this.displayUnits,
            this.locale
        )}${msPostfix}`;
    }

    private async getEndDate() {
        if (this.seconds) {
            return moment().add(this.seconds, "s");
        }
        if (this.endTime) {
            const endDate = await this.getSyncedTime(this.endTime);
            if (endDate) {
                return endDate;
            }
        }
        return moment();
    }

    private async getSyncedTime(time: ReadonlyMoment) {
        const start = window.performance.now();
        const serverTime = await to2(
            this.http.get<{time: moment.Moment}>("/time").toPromise()
        );
        if (!serverTime.ok) {
            return undefined;
        }
        const remaining = time.diff(serverTime.result.time, "ms", true);
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
        const endDate = await this.getSyncedTime(this.endTime);
        // Update the end date only if it was synced successfully with the server
        if (endDate) {
            this.currentEndDate = endDate;
        }
        this.updateSyncInterval();
    }

    ngOnInit() {
        if (!this.autoStart) {
            return;
        }
        void this.start();
    }

    ngOnChanges(c: Changes<this, "endTime">) {
        if (
            c.endTime?.currentValue &&
            !c.endTime.currentValue.isSame(c.endTime.previousValue)
        ) {
            void this.syncEndDate();
        }
    }

    start() {
        void this.startTimer();
    }

    private async startTimer() {
        if (this.currentInterval || this.running) {
            return;
        }
        this.currentEndDate = await this.getEndDate();
        this.updateSyncInterval();
        this.running = true;
        await this.checkCountdown();
        const getIdealInterval = () =>
            this.currentCountdown < DISPLAY_TENTHS_LIMIT
                ? TICK_TENTH_SECOND
                : TICK_SECOND;
        const padInterval = (interval: number) =>
            interval - (moment().valueOf() % interval);
        const getNextInterval = () => padInterval(getIdealInterval());
        const tick = async () => {
            this.onTick.emit(this.currentCountdown);
            if (!this.running) {
                return;
            }
            await this.checkCountdown();
            window.setTimeout(tick, getNextInterval());
        };
        window.setTimeout(tick, getNextInterval());
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
        if (
            this.nextSyncInterval > 0 &&
            now.diff(this.lastSync, "s", true) >= this.nextSyncInterval
        ) {
            await this.syncEndDate();
        }
        const timeEnded = this.currentCountdown <= TIMEOUT_EPS;
        if (
            !this.isLowTime &&
            this.lowTimeThreshold &&
            this.currentCountdown < this.lowTimeThreshold
        ) {
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
            this.nextSyncInterval *=
                1 + (2 * Math.random() - 1) * this.syncIntervalDeviation;
        }
    }
}
